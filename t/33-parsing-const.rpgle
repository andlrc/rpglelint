// rpglelint: -Wundefined-reference
/include 33-parsing-const.rpgleinc
dcl-c C_BANG    const('!');
dcl-c C_AND     const('&');

return C_BANG + C_AND + C_CBEG + C_CEND;
