@host var cpu string
@host var endian symbol
@host var stdin any
@host var stdout any
@host var stderr any
@host var system string
@host var vecBitSize int

@host
type File object:
  @host func close(self) none
  @host func iterator(self) any
  @host func next(self) any
  @host func read(self, n int) any
  @host func readToEnd(self) any
  @host func seek(self, n int) any
  @host func seekFromCur(self, n int) any
  @host func seekFromEnd(self, n int) any
  @host func stat(self) any
  @host func streamLines(self) any
  @host func streamLines(self, bufSize int) any
  @host func write(self, val any) any

@host
type Dir object:
  @host func iterator(self) any
  @host func stat(self) any
  @host func walk(self) any

@host
type DirIterator object:
  @host func next(self) any

@host func access(path any, mode symbol) any
@host func args() List
@host func bindLib(path any, decls List) any
@host func bindLib(path any, decls List, config Map) any
@host func cacheUrl(url any) any
@host func copyFile(srcPath any, dstPath any) any
@host func createDir(path any) any
@host func createFile(path any, truncate boolean) any
@host func cstr(s any) pointer
@host func cwd() string
@host func dirName(path any) any
@host func execCmd(args List) any
@host func exePath() string
@host func exit(status int) none
@host func fetchUrl(url any) any
@host func free(ptr pointer) none
@host func fromCstr(ptr pointer) rawstring
@host func getEnv(key any) any
@host func getEnvAll() Map
@host func getInput() any
@host func malloc(size int) pointer
@host func milliTime() float
@host func openDir(path any) any
@host func openDir(path any, iterable boolean) any
@host func openFile(path any, mode symbol) any
@host func parseArgs(options List) Map
@host func readAll() any
@host func readFile(path any) any
@host func readLine() any
@host func realPath(path any) any
@host func removeDir(path any) any
@host func removeFile(path any) any
@host func setEnv(key any, val any) none
@host func sleep(ms float) none
@host func unsetEnv(key any) none
@host func writeFile(path any, contents any) any