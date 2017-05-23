package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.BuiltInFunction;
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
public class WriteOut extends BuiltInFunction {

    private static final long serialVersionUID = 7090589580229750373L;

    /**
     * Creates new instance of writeout special form
     */
    public WriteOut() {
        super("writeout", "&rest", "arguments");
    }

    @SuppressWarnings("UseOfSystemOutOrSystemErr")
    @Override
    public LispValue applyFunction(final Context context, final LispCollection parameters) {
        boolean first = true;
        for (final LispValue value : parameters) {
            if (first) {
                first = false;
            } else {
                System.out.print(" ");
            }
            System.out.print(value.repr());
        }
        return NilCollection.INSTANCE;
    }
}
