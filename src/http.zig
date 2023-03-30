const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

const Request = if (!cy.isWasm) std.http.Client.Request else void;
const StdResponse = if (!cy.isWasm) std.http.Client.Response else void;

/// Interface to http client.
pub const HttpClient = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    const VTable = struct {
        // request: *const fn (ptr: *anyopaque, uri: std.Uri) RequestError!Request,
        // Using `anyerror` because of dependency loop issue with zig master.
        request: *const fn (ptr: *anyopaque, uri: std.Uri) anyerror!Request,
        // readAll: *const fn (ptr: *anyopaque, req: *Request, buf: []u8) RequestError!usize,
        readAll: *const fn (ptr: *anyopaque, req: *Request, buf: []u8) anyerror!usize,
    };

    pub fn request(self: HttpClient, uri: std.Uri) !Request {
        return self.vtable.request(self.ptr, uri);
    }

    // pub fn readAll(self: HttpClient, req: *Request, buf: []u8) RequestError!usize {
    pub fn readAll(self: HttpClient, req: *Request, buf: []u8) anyerror!usize {
        return self.vtable.readAll(self.ptr, req, buf);
    }
};

const RequestError = std.http.Client.Request.ReadError;

pub const StdHttpClient = struct {
    client: std.http.Client,

    pub fn init(alloc: std.mem.Allocator) StdHttpClient {
        return StdHttpClient{
            .client = .{
                .allocator = alloc,
            },
        };
    }
    
    pub fn deinit(self: *StdHttpClient) void {
        self.client.deinit();
    }

    pub fn iface(self: *StdHttpClient) HttpClient {
        return HttpClient{
            .ptr = self,
            .vtable = &.{
                .request = request,
                .readAll = readAll,
            },
        };
    }

    fn request(ptr: *anyopaque, uri: std.Uri) RequestError!Request {
        const self = stdx.ptrAlignCast(*StdHttpClient, ptr);
        return stdRequest(&self.client, uri, .{}, .{});
    }

    fn readAll(_: *anyopaque, req: *Request, buf: []u8) RequestError!usize {
        return req.readAll(buf);
    }
};

pub const MockHttpClient = struct {
    retReqError: ?RequestError = null,
    retStatusCode: ?std.http.Status = null,
    retBody: []const u8 = "Hello.",
    retBodyIdx: usize = undefined,
    readResponseHeaders: bool = undefined, 

    pub fn init() MockHttpClient {
        return .{};
    }
    
    pub fn iface(self: *MockHttpClient) HttpClient {
        return HttpClient{
            .ptr = self,
            .vtable = &.{
                .request = request,
                .readAll = readAll,
            },
        };
    }

    fn request(ptr: *anyopaque, uri: std.Uri) RequestError!Request {
        const self = stdx.ptrAlignCast(*MockHttpClient, ptr);
        self.readResponseHeaders = false;
        if (self.retReqError) |err| {
            return err;
        } else {
            self.retBodyIdx = 0;
            var req = Request{
                .uri = uri,
                .client = undefined,
                .connection = undefined,
                .redirects_left = undefined,
                .response = undefined,
                .headers = undefined,
                .handle_redirects = undefined,
                .compression_init = undefined,
                .arena = undefined,
            };
            req.response.compression = .none;
            req.response.done = true;

            req.arena = std.heap.ArenaAllocator.init(undefined);
            return req;
        }
    }

    fn readAll(ptr: *anyopaque, req: *Request, buf: []u8) RequestError!usize {
        const self = stdx.ptrAlignCast(*MockHttpClient, ptr);
        if (!self.readResponseHeaders) {
            // First read consumes response headers.
            if (self.retStatusCode) |code| {
                req.response.headers.status = code;
            } else {
                req.response.headers.status = .ok;
            }
            self.readResponseHeaders = true;
        }
        if (self.retBodyIdx < self.retBody.len) {
            const n = std.math.min(buf.len, self.retBody.len - self.retBodyIdx);
            std.mem.copy(u8, buf, self.retBody[self.retBodyIdx..self.retBodyIdx+n]);
            self.retBodyIdx += n;
            return n;
        } else {
            return 0;
        }
    }
};

const Response = struct {
    status: std.http.Status,
    body: []const u8,
};

/// HTTP GET, always cosumes body.
pub fn get(alloc: std.mem.Allocator, client: HttpClient, url: []const u8) !Response {
    const uri = try std.Uri.parse(url);
    var req = try client.request(uri);
    defer req.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    errdefer buf.deinit(alloc);
    var readBuf: [4096]u8 = undefined;

    while (true) {
        const read = try client.readAll(&req, &readBuf);
        try buf.appendSlice(alloc, readBuf[0..read]);
        if (read == 0) {
            break;
        }
    }
    return Response{
        .status = req.response.headers.status,
        .body = try buf.toOwnedSlice(alloc),
    };
}

/// std/http/Client.request
fn stdRequest(client: *std.http.Client, uri: std.Uri, headers: std.http.Client.Request.Headers, options: std.http.Client.Request.Options) !std.http.Client.Request {
    const protocol: std.http.Client.Connection.Protocol = if (std.mem.eql(u8, uri.scheme, "http"))
        .plain
    else if (std.mem.eql(u8, uri.scheme, "https"))
        .tls
    else
        return error.UnsupportedUrlScheme;

    const port: u16 = uri.port orelse switch (protocol) {
        .plain => 80,
        .tls => 443,
    };

    const host = uri.host orelse return error.UriMissingHost;

    if (client.next_https_rescan_certs and protocol == .tls) {
        client.connection_pool.mutex.lock(); // TODO: this could be so much better than reusing the connection pool mutex.
        defer client.connection_pool.mutex.unlock();

        if (client.next_https_rescan_certs) {
            try client.ca_bundle.rescan(client.allocator);
            client.next_https_rescan_certs = false;
        }
    }

    var req: Request = .{
        .uri = uri,
        .client = client,
        .headers = headers,
        .connection = try client.connect(host, port, protocol),
        .redirects_left = options.max_redirects,
        .handle_redirects = options.handle_redirects,
        .compression_init = false,
        .response = switch (options.header_strategy) {
            .dynamic => |max| StdResponse.initDynamic(max),
            .static => |buf| StdResponse.initStatic(buf),
        },
        .arena = undefined,
    };

    req.arena = std.heap.ArenaAllocator.init(client.allocator);

    {
        var buffered = std.io.bufferedWriter(req.connection.data.writer());
        const writer = buffered.writer();

        const escaped_path = try std.Uri.escapePath(client.allocator, uri.path);
        defer client.allocator.free(escaped_path);

        const escaped_query = if (uri.query) |q| try std.Uri.escapeQuery(client.allocator, q) else null;
        defer if (escaped_query) |q| client.allocator.free(q);

        const escaped_fragment = if (uri.fragment) |f| try std.Uri.escapeQuery(client.allocator, f) else null;
        defer if (escaped_fragment) |f| client.allocator.free(f);

        try writer.writeAll(@tagName(headers.method));
        try writer.writeByte(' ');
        if (escaped_path.len == 0) {
            try writer.writeByte('/');
        } else {
            try writer.writeAll(escaped_path);
        }
        if (escaped_query) |q| {
            try writer.writeByte('?');
            try writer.writeAll(q);
        }
        if (escaped_fragment) |f| {
            try writer.writeByte('#');
            try writer.writeAll(f);
        }
        try writer.writeByte(' ');
        try writer.writeAll(@tagName(headers.version));
        try writer.writeAll("\r\nHost: ");
        try writer.writeAll(host);
        try writer.writeAll("\r\nUser-Agent: ");
        try writer.writeAll(headers.user_agent);
        if (headers.connection == .close) {
            try writer.writeAll("\r\nConnection: close");
        } else {
            try writer.writeAll("\r\nConnection: keep-alive");
        }
        try writer.writeAll("\r\nAccept-Encoding: gzip, deflate, zstd");

        switch (headers.transfer_encoding) {
            .chunked => try writer.writeAll("\r\nTransfer-Encoding: chunked"),
            .content_length => |content_length| try writer.print("\r\nContent-Length: {d}", .{content_length}),
            .none => {},
        }

        for (headers.custom) |header| {
            try writer.writeAll("\r\n");
            try writer.writeAll(header.name);
            try writer.writeAll(": ");
            try writer.writeAll(header.value);
        }

        try writer.writeAll("\r\n\r\n");

        try buffered.flush();
    }

    return req;
}
