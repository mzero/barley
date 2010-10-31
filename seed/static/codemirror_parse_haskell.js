var HaskellParser = Editor.Parser = (function() {
  var tokenizeHaskell = (function() {
  
    // These should all be Unicode extended
    var smallRE = /[a-z_]/;
    var largeRE = /[A-Z]/;
    var digitRE = /[0-9]/;
    var hexitRE = /[0-9A-Fa-f]/;
    var octitRE = /[0-7]/;
    var idRE = /[a-z_A-Z0-9']/;
    var symbolRE = /[-!#$%&*+.\/<=>?@\\^|~:]/;
    
    function normal(source, setState) {

      function nextIfLiteralChar(delimiter) {
        if (source.endOfLine()) return false;
        if (source.equals(delimiter)) return false;
        if (source.equals('\\')) {
          source.next();
          source.next();
        }
        else {
          source.next();
        }
        return true
      }
      
      var ch = source.next();
      
      if (/[(),;[\]`{}]/.test(ch)) {
        if (ch == '{' && source.equals('-')) {
          source.next();
          var t = "hs-comment";
          if (source.equals('#')) {
            source.next();
            t = "hs-pragma";
          }
          setState(ncomment(t, 1));
          return null;
        }
        return "hs-special";
      }
      
      if (ch == '\'') {
        if (source.next == '\\') {
          source.next();  // should do more here
          if (source.equals('\'')) {
            source.next();
            return "hs-char";
          }
        }
        return "hs-error";
      }
      
      if (ch == '"') {
        setState(stringLiteral);
        return null;
      }
      
      if (largeRE.test(ch)) {
        source.nextWhileMatches(idRE);
        if (source.equals('.')) {
          source.next();
          return "hs-qualifier";
        }
        return "hs-conid";
      }
      
      if (smallRE.test(ch)) {
        source.nextWhileMatches(idRE);
        return "hs-varid";
      }
      
      if (digitRE.test(ch)) {
        if (ch == '0') {
          if (source.matches(/[xX]/)) {
            source.next();
            source.nextWhileMatches(hexitRE); // should require at least 1
            return "hs-integer";
          }
          if (source.matches(/[oO]/)) {
            source.next();
            source.nextWhileMatches(octitRE); // should require at least 1
            return "hs-integer";
          }
        }
        source.nextWhileMatches(digitRE);
        var t = "hs-integer";
        if (source.equals('.')) {
          t = "hs-float";
          source.next();
          source.nextWhileMatches(digitRE); // should require at least 1
        }
        if (source.matches(/[eE]/)) {
          t = "hs-float";
          source.next();
          if (source.matches(/[-+]/)) {
            source.next();
          }
          source.nextWhileMatches(digitRE); // should require at least 1
        }
        return t;
      }
      
      if (symbolRE.test(ch)) {
        if (ch == '-' && source.matches(/-/)) {
          source.nextWhileMatches(/-/);
          if (!source.matches(symbolRE)) {
            while (!source.endOfLine()) {
              source.next();
            }
            return "hs-comment";
          }
        }
        var t = "hs-varsym";
        if (ch == ':') {
          t = "hs-consym";
        }
        source.nextWhileMatches(symbolRE);
        return t;    
      }
      
      return "hs-error";
    }
    
    function in_ncomment(type, nest) {
      if (nest == 0) {
        return normal;
      }
      return function(source, setState) {
        while (source.more() && !source.endOfLine()) {
          ch = source.next();
          if (ch == '{' && source.equals('-')) {
            source.next();
            setState(ncomment(type, nest+1));
            return null;
          }
          else if (ch == '-' && source.equals('}')) {
            source.next();
            setState(ncomment(type, nest-1));
            return (nest == 1) ? type : null;
          }
        }
        return type;
      }
    }
    
    whiteCharRE = /[ \t\v\f]/; // newlines are handled in tokenizer
    
    function stringLiteral(source, setState) {
      while (!source.endOfLine()) {
        var ch = source.next();
        if (ch == '"') {
          setState(normal);
          return "hs-string";
        }
        if (ch == '\\') {
          if (source.endOfLine() || source.matches(whiteCharRE)) {
            setState(stringGap);
            return "hs-string";
          }
          if (source.equals('&')) {
            source.next();
          }
          else {
            source.next(); // could do more complicated matching here
          }
        }
      }
      setState(normal);
      return "hs-error";
    }
    
    function stringGap(source, setState) {
      if (source.equals('\\')) {
        source.next();
        setState(stringLiteral);
        return null; //"hs-gap";
      }
      source.next();
      setState(normal);
      return "hs-error";
    }
    
    return function(source, state) {
      return tokenizer(source, state || normal);
    };
  })(); 
  
  var reservedidRE = /^(case|class|data|default|deriving|do|else|foreign|if|import|in|infix|infixl|infixr|instance|let|module|newtype|of|then|type|where|_)$/;
  var reservedopRE = /^(\.\.|:|::|=|\\|\||<-|->|@|~|=>)$/;

  function parseHaskell(source) {
    function indentTo(n) {return function() {return n;}}
    var tokens = tokenizeHaskell(source);
    var space = 0;

    var iter = {
      next: function() {
        var tok = tokens.next();
        if (tok.type == "whitespace") {
          if (tok.value == "\n") tok.indentation = indentTo(space);
          else space = tok.value.length;
        }
        else if (reservedidRE.test(tok.content)) {
          tok.style = "hs-reservedid";
        }
        else if (reservedopRE.test(tok.content)) {
          tok.style = "hs-reservedop";
        }
        return tok;
      },
      copy: function() {
        var _space = space;
        var _tokensState = tokens.state;
        return function(_source) {
          space = _space;
          tokens = tokenizeHaskell(_source, _tokensState);
          return iter;
        };
      }
    };
    return iter;
  }
  return {make: parseHaskell};
})();
