package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.evaluator.Context;

/**
 * (println val1 val2 val3)
 * <p/>
 * prints val1 val2 val3 and returns NIL
 * <p/>
 * Created on 01/03/15.
 */
public class PrintLn extends SpecialForm {

    /**
     * Creates new instance of println special form
     */
    public PrintLn() {
        super("println", "&rest", "arguments");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection evaluated = context.evaluateList(parameters);

        for (final LispValue value : evaluated) {
            System.out.print(value.repr());
            System.out.print(" ");
        }
        System.out.println();
        return NilCollection.INSTANCE;
    }
}
