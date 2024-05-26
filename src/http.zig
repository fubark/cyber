const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

const Request = if (cy.hasCLI) std.http.Client.Request else void;
const StdResponse = if (cy.hasCLI) std.http.Client.Response else void;
const Headers = if (cy.hasCLI) std.http.Client.Request.Headers else void;

const MockError = enum {
    UnknownHostName,
};

/// Interface to http client.
pub const HttpClient = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    const VTable = struct {
        // Using `anyerror` because of dependency loop issue with zig master.
        request: *const fn (ptr: *anyopaque, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Request,
        readAll: *const fn (ptr: *anyopaque, req: *Request, buf: []u8) anyerror!usize,
        deinit: *const fn (ptr: *anyopaque) void,
        deinitRequest: *const fn (ptr: *anyopaque, req: *Request) void,
        startRequest: *const fn (ptr: *anyopaque, req: *Request) anyerror!void,
        waitRequest: *const fn (ptr: *anyopaque, req: *Request) anyerror!void,
        fetch: *const fn (ptr: *anyopaque, alloc: std.mem.Allocator, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Response,
    };

    pub fn fetch(self: HttpClient, alloc: std.mem.Allocator, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Response {
        return self.vtable.fetch(self.ptr, alloc, method, uri, headers);
    }

    pub fn request(self: HttpClient, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Request {
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
                .fetch = fetch,
                .request = request,
                .deinit = deinit,
                .deinitRequest = deinitRequest,
                .startRequest = startRequest,
                .waitRequest = waitRequest,
                .readAll = readAll,
            },
        };
    }

    fn fetch(ptr: *anyopaque, alloc: std.mem.Allocator, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Response {
        const self = cy.ptrAlignCast(*StdHttpClient, ptr);
        var buf = std.ArrayList(u8).init(alloc);
        defer buf.deinit();
        const res = try self.client.fetch(.{
            .response_storage = .{ .dynamic = &buf },
            .method = method,
            .max_append_size = 1e9,
            .location = .{ .uri = uri },
            .headers = headers,
        });
        return Response{
            .status = res.status,
            .body = try buf.toOwnedSlice(),
        };
    }

    fn request(ptr: *anyopaque, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Request {
        _ = ptr;
        _ = method;
        _ = uri;
        _ = headers;
        return error.TODO;  
    }

    fn deinit(ptr: *anyopaque) void {
        const self: *StdHttpClient = @ptrCast(@alignCast(ptr));
        self.client.deinit();
    }

    fn deinitRequest(_: *anyopaque, req: *Request) void {
        req.deinit();
    }

    fn startRequest(_: *anyopaque, req: *Request) anyerror!void {
        _ = req;
    
        return error.TODO;  
        // try req.start();
    }

    fn waitRequest(_: *anyopaque, req: *Request) anyerror!void {
        _ = req;
    
        return error.TODO;  
        // try req.wait();
    }

    fn readAll(_: *anyopaque, req: *Request, buf: []u8) anyerror!usize {
        _ = req;
        _ = buf;
    
        return error.TODO;  
        // return req.reader().readAll(buf);
    }
};

pub const MockHttpClient = struct {
    // A consistent enum is used to avoid error union mismatches between test and lib artifacts.
    retReqError: ?MockError = null,
    retStatusCode: ?std.http.Status = null,
    retBody: []const u8 = "Hello.",
    retBodyIdx: usize = undefined,
    // client: std.http.Client,

    pub fn init(alloc: std.mem.Allocator) MockHttpClient {
        _ = alloc;
        return .{};
        // return .{ .client = .{ .allocator = alloc } };
    }
    
    pub fn iface(self: *MockHttpClient) HttpClient {
        return HttpClient{
            .ptr = self,
            .vtable = &.{
                .fetch = fetch,
                .request = request,
                .deinit = deinit,
                .deinitRequest = deinitRequest,
                .startRequest = startRequest,
                .waitRequest = waitRequest,
                .readAll = readAll,
            },
        };
    }

    fn fetch(ptr: *anyopaque, alloc: std.mem.Allocator, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Response {
        _ = method;
        _ = uri;
        _ = headers;
        const self = cy.ptrAlignCast(*MockHttpClient, ptr);
        if (self.retReqError) |err| {
            switch (err) {
                .UnknownHostName => return error.UnknownHostName,
            }
        }

        return Response{
            .status = self.retStatusCode orelse .ok,
            .body = try alloc.dupe(u8, self.retBody),
        };
    }

    fn request(ptr: *anyopaque, method: std.http.Method, uri: std.Uri, headers: Headers) anyerror!Request {
        _ = ptr;
        _ = method;
        _ = uri;
        _ = headers;
    
        // const self = cy.ptrAlignCast(*MockHttpClient, ptr);
        // if (self.retReqError) |err| {
        //     return err;
        // } else {
        //     self.retBodyIdx = 0;
        //     const options: std.http.Client.Options = .{};
        //     var req = Request{
        //         .uri = uri,
        //         .client = &self.client,
        //         .connection = undefined,
        //         .redirects_left = undefined,
        //         .headers = headers,
        //         .method = method,
        //         .handle_redirects = undefined,
        //         .arena = undefined,
        //         .response = .{
        //             .status = undefined,
        //             .reason = undefined,
        //             .version = undefined,
        //             .headers = Headers{ .allocator = self.client.allocator, .owned = false },
        //             .parser = switch (options.header_strategy) {
        //                 .dynamic => |max| std.http.protocol.HeadersParser.initDynamic(max),
        //                 .static => |buf| std.http.protocol.HeadersParser.initStatic(buf),
        //             },
        //         },
        //     };
        //     req.response.compression = .none;
        //     req.response.parser.done = true;
        //     req.response.parser.state = .finished;

        //     req.arena = std.heap.ArenaAllocator.init(undefined);
        //     return req;
        // }
        return error.TODO;
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
        _ = ptr;
        _ = req;
    
        // const self: *MockHttpClient = @ptrCast(@alignCast(ptr));
        // if (self.retStatusCode) |code| {
        //     req.response.status = code;
        // } else {
        //     req.response.status = .ok;
        // }
        return error.TODO;
    }

    fn readAll(ptr: *anyopaque, _: *Request, buf: []u8) anyerror!usize {
        _ = ptr;
        _ = buf;
    
        // const self: *MockHttpClient = @ptrCast(@alignCast(ptr));
        // if (self.retBodyIdx < self.retBody.len) {
        //     const n = @min(buf.len, self.retBody.len - self.retBodyIdx);
        //     std.mem.copy(u8, buf, self.retBody[self.retBodyIdx..self.retBodyIdx+n]);
        //     self.retBodyIdx += n;
        //     return n;
        // } else {
        //     return 0;
        // }
        return error.TODO;
    }
};

const Response = struct {
    status: std.http.Status,
    body: []const u8,
};

/// HTTP GET, always cosumes body.
pub fn get(alloc: std.mem.Allocator, client: HttpClient, url: []const u8) !Response {
    const uri = try std.Uri.parse(url);
    return client.fetch(alloc, .GET, uri, .{});
}