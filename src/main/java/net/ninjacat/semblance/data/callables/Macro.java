package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.errors.runtime.UnboundSymbolException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.smooth.collections.Lists;
import net.ninjacat.smooth.iterators.Collectors;
import net.ninjacat.smooth.iterators.Iter;
import net.ninjacat.smooth.utils.Option;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

public class Macro implements Callable {

    private final SymbolAtom name;
    private final LispCollection params;
    private final LispCollection body;
    private final Parameters formalParameters;

    public Macro(final SymbolAtom name, final LispCollection params,
                 final LispCollection body) {
        this.name = name;
        this.params = params;
        formalParameters = new Parameters(asSList(params));
        this.body = body;
    }

    @Override
    public SymbolAtom name() {
        return name;
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context parameterContext = LocalContext.namelessChildContext(context);
        final Context executionContext = LocalContext.namedChildContext(name.repr(), context);

        formalParameters.apply(parameterContext, parameters);

        final LispCollection expandedBody = expandMacro(body, parameterContext);
        return executionContext.evaluateBlock(expandedBody);
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.MACRO;
    }

    @Override
    public String repr() {
        return String.format("(defmacro %s %s %s)", name.repr(), params.repr(), body.repr());
    }

    private LispCollection expandMacro(final LispCollection sExpr, final Context parameterContext) {
        final List<LispValue> output = new ArrayList<>();

        for (final LispValue item : sExpr) {
            if (isCollection(item)) {
                output.add(expandMacro(asCollection(item), parameterContext));
            } else {
                if (isSymbol(item) && isMacroParameter(item)) {
                    output.addAll(expandParameter(item, parameterContext));
                } else {
                    output.add(item);
                }
            }
        }

        return new SList(output);
    }

    private boolean isMacroParameter(final LispValue item) {
        return asSymbol(item).repr().startsWith(",");
    }

    private Collection<LispValue> expandParameter(final LispValue item, final Context parameterContext) {
        final String paramName = item.repr().substring(1);
        final boolean isExpandable = paramName.startsWith("@");
        final SymbolAtom actualName = new SymbolAtom(isExpandable ? paramName.substring(1) : paramName);
        final Option<LispValue> value = parameterContext.findSymbol(actualName);
        if (value.isPresent()) {
            if (isExpandable) {
                return expandList(value.get());
            } else {
                return Lists.of(value.get());
            }
        } else {
            throw new UnboundSymbolException(asSymbol(item), getSourceInfo(item));
        }
    }

    private Collection<LispValue> expandList(final LispValue value) {
        return Iter.of(asCollection(value).iterator()).collectWith(Collectors.<LispValue>arrayList());
    }
}
