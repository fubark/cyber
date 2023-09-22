hostfunc access(path any, mode symbol) any
hostfunc args() List
hostfunc bindLib(path any, decls List) any
hostfunc bindLib(path any, decls List, config Map) any
hostfunc cacheUrl(url any) any
hostfunc copyFile(srcPath any, dstPath any) any
hostfunc createDir(path any) any
hostfunc createFile(path any, truncate boolean) any
hostfunc cstr(s any) pointer
hostfunc cwd() string
hostfunc dirName(path any) any
hostfunc execCmd(args List) any
hostfunc exePath() string
hostfunc exit(status int) none
hostfunc fetchUrl(url any) any
hostfunc free(ptr pointer) none
hostfunc fromCstr(ptr pointer) rawstring
hostfunc getEnv(key any) any
hostfunc getEnvAll() Map
hostfunc getInput() any
hostfunc malloc(size int) pointer
hostfunc milliTime() float
hostfunc openDir(path any) any
hostfunc openDir(path any, iterable boolean) any
hostfunc openFile(path any, mode symbol) any
hostfunc parseArgs(options List) Map
hostfunc readAll() any
hostfunc readFile(path any) any
hostfunc readLine() any
hostfunc realPath(path any) any
hostfunc removeDir(path any) any
hostfunc removeFile(path any) any
hostfunc setEnv(key any, val any) none
hostfunc sleep(ms float) none
hostfunc unsetEnv(key any) none
hostfunc writeFile(path any, contents any) any