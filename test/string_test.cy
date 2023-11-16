import t 'test'

var escaped = 'Return the underlying `symbol`.'.replace('`', '\\`')
t.eq(escaped, 'Return the underlying \\`symbol\\`.')