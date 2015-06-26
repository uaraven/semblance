package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SpecialValue;
import net.ninjacat.semblance.data.collections.*;
import net.ninjacat.semblance.data.collections.Vector;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.UnknownExpressionRuntimeException;
import net.ninjacat.semblance.errors.runtime.UnexpectedEndRuntimeException;
import net.ninjacat.semblance.reader.converters.TokenConverter;

import java.util.*;


class ParserIterator implements Iterator<LispValue> {

    private final Iterator<Token> tokens;
    private final Map<String, ReaderMacro> macros;
    private final Map<Token.TokenType, TokenConverter> converters;
    private SourceInfo lastSourceInfo;

    ParserIterator(
            final Iterator<Token> tokens,
            final Map<Token.TokenType, TokenConverter> converters,
            final Map<String, ReaderMacro> macros) {
        this.tokens = tokens;
        this.macros = Collections.unmodifiableMap(macros);
        this.converters = Collections.unmodifiableMap(converters);
        lastSourceInfo = SourceInfo.UNKNOWN;
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
            final Token token = tokens.next();
            lastSourceInfo = token.getSourceInfo();
            switch (token.getType()) {
                case CloseParens:
                case CloseBracket:
                    return SpecialValue.LIST_END;
                case CloseBrace:
                    return SpecialValue.MAP_END;
                case Eof:
                    return SpecialValue.PROGRAM_END;
                case OpenParens:
                    return parseList(token);
                case OpenBracket:
                    return parseVector(token);
                case OpenBrace:
                    return parseMap(token);
                case Special:
                    return parseReaderMacro(token);
                default:
                    return parseAtom(token);
            }
        } else {
            throw new UnexpectedEndRuntimeException(lastSourceInfo);
        }
    }

    private LispValue parseAtom(final Token token) {
        if (converters.containsKey(token.getType())) {
            return converters.get(token.getType()).mkValue(token);
        } else {
            throw new UnknownExpressionRuntimeException(token);
        }
    }

    private LispValue parseReaderMacro(final Token token) {
        if (macros.containsKey(token.getValue())) {
            return macros.get(token.getValue()).replaceReaderMacro(parseInternal());
        } else {
            return parseAtom(token);
        }
    }

    private List<LispValue> parseCollection() {
        final List<LispValue> collection = new LinkedList<>();
        //noinspection LoopConditionNotUpdatedInsideLoop
        while (tokens.hasNext()) {
            final LispValue value = parseInternal();
            if (SemblanceType.SPECIAL == value.getType()) {
                if (SpecialValue.LIST_END.equals(value)) {
                    return collection;
                } else {
                    break;
                }
            }
            collection.add(value);
        }
        throw new UnexpectedEndRuntimeException(lastSourceInfo);
    }

    private SList parseList(final Token token) {
        return new SList(parseCollection(), token.getSourceInfo());
    }

    private Vector parseVector(final Token token) {
        return new Vector(parseCollection(), token.getSourceInfo());
    }

    private SMap parseMap(final Token token) {
        final HashMap<LispValue, LispValue> map = new HashMap<>();
        //noinspection LoopConditionNotUpdatedInsideLoop
        while (tokens.hasNext()) {
            final LispValue key = parseInternal();
            if (SemblanceType.SPECIAL == key.getType()) {
                if (SpecialValue.MAP_END.equals(key)) {
                    return new SMap(map, token.getSourceInfo());
                } else {
                    break;
                }
            }
            final LispValue value = parseInternal();
            if (SemblanceType.SPECIAL == value.getType()) {
                if (SpecialValue.MAP_END.equals(value)) {
                    map.put(key, NilCollection.INSTANCE);
                    return new SMap(map, token.getSourceInfo());
                } else {
                    break;
                }
            }
            map.put(key, value);
        }
        throw new UnexpectedEndRuntimeException(lastSourceInfo);
    }

}
