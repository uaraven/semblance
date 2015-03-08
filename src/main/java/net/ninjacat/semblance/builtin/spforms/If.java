package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.utils.Require;
import net.ninjacat.semblance.utils.Values;

import static net.ninjacat.semblance.utils.Values.asSList;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public class If extends SpecialForm {

    public If() {
        super("if", "cond", "then", "&optional", "else");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasAtLeast(2);

        final Context localContext = LocalContext.namelessChildContext(context);

        final SList condition = asSList(parameters.head());
        Require.that(condition).hasExactly(1);

        final LispValue thenValue = parameters.tail().head();
        final LispValue elseValue = parameters.tail().tail().isNil()
                ? NilCollection.INSTANCE
                : parameters.tail().tail().head();

        final LispValue evaluated = localContext.evaluate(condition.head());
        if (Values.isTrue(evaluated)) {
            return localContext.evaluate(thenValue);
        } else if (!elseValue.equals(NilCollection.INSTANCE)) {
            return localContext.evaluate(elseValue);
        } else {
            return NilCollection.INSTANCE;
        }
    }
}
