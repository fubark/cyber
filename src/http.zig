const std = @import("std");
const stdx = @import("stdx");
const cy = @import("cyber.zig");

const Request = if (!cy.isWasm) std.http.Client.Request else void;

/// Interface to http client.
pub const HttpClient = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    const VTable = struct {
        request: *const fn (ptr: *anyopaque, uri: std.Uri) RequestError!Request,
        readAll: *const fn (ptr: *anyopaque, req: *Request, buf: []u8) RequestError!usize,
    };

    pub fn request(self: HttpClient, uri: std.Uri) !Request {
        return self.vtable.request(self.ptr, uri);
    }

    pub fn readAll(self: HttpClient, req: *Request, buf: []u8) RequestError!usize {
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
        return self.client.request(uri, .{}, .{});
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
        _ = uri;
        const self = stdx.ptrAlignCast(*MockHttpClient, ptr);
        self.readResponseHeaders = false;
        if (self.retReqError) |err| {
            return err;
        } else {
            self.retBodyIdx = 0;
            return Request{
                .client = undefined,
                .connection = undefined,
                .redirects_left = undefined,
                .response = undefined,
                .headers = undefined,
            };
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