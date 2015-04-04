package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.UnknownExpressionException;
import net.ninjacat.semblance.reader.converters.*;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Created on 27/02/15.
 */
class Parser {

    private static final Map<Token.TokenType, TokenConverter> CONVERTERS = new HashMap<>();
    private final Map<String, ReaderMacro> readerMacros;

    Parser() {
        readerMacros = new ConcurrentHashMap<>();
    }

    void registerReaderMacro(final ReaderMacro macro) {
        readerMacros.put(macro.getMacroCharacter(), macro);
    }

    SList parse(final List<Token> tokens) throws UnknownExpressionException {
        final ParserIterator parserIterator = new ParserIterator(tokens.iterator(), CONVERTERS, readerMacros);
        final List<LispValue> result = new LinkedList<>();
        while (parserIterator.hasNext()) {
            result.add(parserIterator.next());
        }
        return new SList(result, tokens.isEmpty() ? SourceInfo.UNKNOWN : tokens.get(0).getSourceInfo());
    }

    static {
        CONVERTERS.put(Token.TokenType.Symbol, new SymbolConverter());
        CONVERTERS.put(Token.TokenType.String, new StringConverter());
        CONVERTERS.put(Token.TokenType.Integer, new NumberConverter());
        CONVERTERS.put(Token.TokenType.Double, new DoubleConverter());
    }

}
