package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.data.Vector;
import net.ninjacat.semblance.errors.UnexpectedEndRuntimeException;
import net.ninjacat.semblance.errors.UnknownExpressionRuntimeException;
import net.ninjacat.semblance.reader.converters.TokenConverter;

import java.util.*;

/**
 * Created on 27/02/15.
 */
public class ParserIterator implements Iterator<LispValue> {

    private final Iterator<Token> tokens;
    private final Map<String, ReaderMacro> macros;
    private final Map<Token.TokenType, TokenConverter> converters;

    public ParserIterator(
            Iterator<Token> tokens,
            Map<Token.TokenType, TokenConverter> converters,
            Map<String, ReaderMacro> macros) {
        this.tokens = tokens;
        this.macros = Collections.unmodifiableMap(macros);
        this.converters = Collections.unmodifiableMap(converters);
    }

    @Override
    public boolean hasNext() {
        return tokens.hasNext();
    }

    @Override
    public LispValue next() {
        return parseInternal();
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    private LispValue parseInternal() {
        if (tokens.hasNext()) {
            Token token = tokens.next();
            switch (token.getType()) {
                case CloseParens:
                case CloseBracket:
                    return SpecialValue.LIST_END;
                case Eof:
                    return SpecialValue.PROGRAM_END;
                case OpenParens:
                    return parseList(token);
                case OpenBracket:
                    return parseVector(token);
                case Special:
                    return parseReaderMacro(token);
                default:
                    return parseAtom(token);
            }
        } else {
            throw new UnexpectedEndRuntimeException();
        }
    }

    private LispValue parseAtom(Token token) {
        if (converters.containsKey(token.getType())) {
            return converters.get(token.getType()).mkValue(token);
        } else {
            throw new UnknownExpressionRuntimeException(token);
        }
    }

    private LispValue parseReaderMacro(Token token) {
        if (macros.containsKey(token.getValue())) {
            return macros.get(token.getValue()).replaceReaderMacro(parseInternal());
        } else {
            return parseAtom(token);
        }
    }

    private List<LispValue> parseCollection() {
        List<LispValue> collection = new LinkedList<>();
        while (tokens.hasNext()) {
            LispValue value = parseInternal();
            if (value.getType() == SemblanceType.SPECIAL) {
                if (value == SpecialValue.LIST_END) {
                    return collection;
                } else {
                    break;
                }
            }
            collection.add(value);
        }
        throw new UnexpectedEndRuntimeException();
    }

    private SList parseList(Token token) {
        return new SList(parseCollection(), token.getSourceInfo());
    }

    private Vector parseVector(Token token) {
        return new Vector(parseCollection(), token.getSourceInfo());
    }

}
