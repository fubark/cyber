const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

const Request = if (cy.hasCLI) std.http.Client.Request else void;
const StdResponse = if (cy.hasCLI) std.http.Client.Response else void;

/// Interface to http client.
pub const HttpClient = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    const VTable = struct {
        // Using `anyerror` because of dependency loop issue with zig master.
        request: *const fn (ptr: *anyopaque, method: std.http.Method, uri: std.Uri, headers: std.http.Headers) anyerror!Request,
        readAll: *const fn (ptr: *anyopaque, req: *Request, buf: []u8) anyerror!usize,
        deinit: *const fn (ptr: *anyopaque) void,
        deinitRequest: *const fn (ptr: *anyopaque, req: *Request) void,
        startRequest: *const fn (ptr: *anyopaque, req: *Request) anyerror!void,
        waitRequest: *const fn (ptr: *anyopaque, req: *Request) anyerror!void,
    };

    pub fn request(self: HttpClient, method: std.http.Method, uri: std.Uri, headers: std.http.Headers) !Request {
        return self.vtable.request(self.ptr, method, uri, headers);
    }

    pub fn deinit(self: HttpClient) void {
        return self.vtable.deinit(self.ptr);
    }

    pub fn deinitRequest(self: HttpClient, req: *Request) void {
        return self.vtable.deinitRequest(self.ptr, req);
    }

    pub fn startRequest(self: HttpClient, req: *Request) anyerror!void {
        try self.vtable.startRequest(self.ptr, req);
    }

    pub fn waitRequest(self: HttpClient, req: *Request) anyerror!void {
        try self.vtable.waitRequest(self.ptr, req);
    }

    pub fn readAll(self: HttpClient, req: *Request, buf: []u8) anyerror!usize {
        return self.vtable.readAll(self.ptr, req, buf);
    }
};

const RequestError = std.http.Client.RequestError;

pub const StdHttpClient = struct {
    client: std.http.Client,

    pub fn init(alloc: std.mem.Allocator) StdHttpClient {
        return StdHttpClient{
            .client = .{
                .allocator = alloc,
            },
        };
    }
    
    pub fn iface(self: *StdHttpClient) HttpClient {
        return HttpClient{
            .ptr = self,
            .vtable = &.{
                .request = request,
                .deinit = deinit,
                .deinitRequest = deinitRequest,
                .startRequest = startRequest,
                .waitRequest = waitRequest,
                .readAll = readAll,
            },
        };
    }

    fn request(ptr: *anyopaque, method: std.http.Method, uri: std.Uri, headers: std.http.Headers) RequestError!Request {
        const self = cy.ptrAlignCast(*StdHttpClient, ptr);
        return stdRequest(&self.client, method, uri, headers, .{});
    }

    fn deinit(ptr: *anyopaque) void {
        const self: *StdHttpClient = @ptrCast(@alignCast(ptr));
        self.client.deinit();
    }

    fn deinitRequest(_: *anyopaque, req: *Request) void {
        req.deinit();
    }

    fn startRequest(_: *anyopaque, req: *Request) anyerror!void {
        try req.start();
    }

    fn waitRequest(_: *anyopaque, req: *Request) anyerror!void {
        try req.wait();
    }

    fn readAll(_: *anyopaque, req: *Request, buf: []u8) anyerror!usize {
        return req.reader().readAll(buf);
    }
};

pub const MockHttpClient = struct {
    retReqError: ?RequestError = null,
    retStatusCode: ?std.http.Status = null,
    retBody: []const u8 = "Hello.",
    retBodyIdx: usize = undefined,
    client: std.http.Client,

    pub fn init(alloc: std.mem.Allocator) MockHttpClient {
        return .{ .client = .{ .allocator = alloc } };
    }
    
    pub fn iface(self: *MockHttpClient) HttpClient {
        return HttpClient{
            .ptr = self,
            .vtable = &.{
                .request = request,
                .deinit = deinit,
                .deinitRequest = deinitRequest,
                .startRequest = startRequest,
                .waitRequest = waitRequest,
                .readAll = readAll,
            },
        };
    }

    fn request(ptr: *anyopaque, method: std.http.Method, uri: std.Uri, headers: std.http.Headers) RequestError!Request {
        const self = cy.ptrAlignCast(*MockHttpClient, ptr);
        if (self.retReqError) |err| {
            return err;
        } else {
            self.retBodyIdx = 0;
            const options: std.http.Client.Options = .{};
            var req = Request{
                .uri = uri,
                .client = &self.client,
                .connection = undefined,
                .redirects_left = undefined,
                .headers = headers,
                .method = method,
                .handle_redirects = undefined,
                .arena = undefined,
                .response = .{
                    .status = undefined,
                    .reason = undefined,
                    .version = undefined,
                    .headers = std.http.Headers{ .allocator = self.client.allocator, .owned = false },
                    .parser = switch (options.header_strategy) {
                        .dynamic => |max| std.http.protocol.HeadersParser.initDynamic(max),
                        .static => |buf| std.http.protocol.HeadersParser.initStatic(buf),
                    },
                },
            };
            req.response.compression = .none;
            req.response.parser.done = true;
            req.response.parser.state = .finished;

            req.arena = std.heap.ArenaAllocator.init(undefined);
            return req;
        }
    }

    fn deinit(ptr: *anyopaque) void {
        _ = ptr;
    }

    fn deinitRequest(_: *anyopaque, req: *Request) void {
        _ = req;
    }

    fn startRequest(_: *anyopaque, req: *Request) anyerror!void {
        _ = req;
    }

    fn waitRequest(ptr: *anyopaque, req: *Request) anyerror!void {
        const self: *MockHttpClient = @ptrCast(@alignCast(ptr));
        if (self.retStatusCode) |code| {
            req.response.status = code;
        } else {
            req.response.status = .ok;
        }
    }

    fn readAll(ptr: *anyopaque, _: *Request, buf: []u8) anyerror!usize {
        const self: *MockHttpClient = @ptrCast(@alignCast(ptr));
        if (self.retBodyIdx < self.retBody.len) {
            const n = @min(buf.len, self.retBody.len - self.retBodyIdx);
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
    var req = try client.request(.GET, uri, .{ .allocator = alloc });
    defer req.deinit();

    try client.startRequest(&req);
    try client.waitRequest(&req);

    var buf: std.ArrayListUnmanaged(u8) = .{};
    errdefer buf.deinit(alloc);
    var readBuf: [4096]u8 = undefined;

    while (true) {
        const read = try client.readAll(&req, &readBuf);
        try buf.appendSlice(alloc, readBuf[0..read]);
        if (read < readBuf.len) {
            break;
        }
    }
    return Response{
        .status = req.response.status,
        .body = try buf.toOwnedSlice(alloc),
    };
}

/// std/http/Client.request
fn stdRequest(client: *std.http.Client, method: std.http.Method, uri: std.Uri, headers: std.http.Headers, options: std.http.Client.Options) !std.http.Client.Request {
    const protocol = std.http.Client.protocol_map.get(uri.scheme) orelse return error.UnsupportedUrlScheme;

    const port: u16 = uri.port orelse switch (protocol) {
        .plain => 80,
        .tls => 443,
    };

    const host = uri.host orelse return error.UriMissingHost;

    if (protocol == .tls and @atomicLoad(bool, &client.next_https_rescan_certs, .Acquire)) {
        client.ca_bundle_mutex.lock();
        defer client.ca_bundle_mutex.unlock();

        if (client.next_https_rescan_certs) {
            client.ca_bundle.rescan(client.allocator) catch return error.CertificateBundleLoadFailure;
            @atomicStore(bool, &client.next_https_rescan_certs, false, .Release);
        }
    }

    const conn = options.connection orelse try client.connect(host, port, protocol);

    var req: Request = .{
        .uri = uri,
        .client = client,
        .connection = conn,
        .headers = headers,
        .method = method,
        .version = options.version,
        .redirects_left = options.max_redirects,
        .handle_redirects = options.handle_redirects,
        .response = .{
            .status = undefined,
            .reason = undefined,
            .version = undefined,
            .headers = std.http.Headers{ .allocator = client.allocator, .owned = false },
            .parser = switch (options.header_strategy) {
                .dynamic => |max| std.http.protocol.HeadersParser.initDynamic(max),
                .static => |buf| std.http.protocol.HeadersParser.initStatic(buf),
            },
        },
        .arena = undefined,
    };
    errdefer req.deinit();

    req.arena = std.heap.ArenaAllocator.init(client.allocator);

    return req;
}
