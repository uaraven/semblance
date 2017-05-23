package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.evaluator.Context;

import java.util.stream.Collectors;

/**
 * (println val1 val2 val3)
 * <p/>
 * prints val1 val2 val3 and returns NIL
 * <p/>
 * Created on 01/03/15.
 */
public class WriteOutCr extends BuiltInFunction {

    private static final long serialVersionUID = 7090589580229750373L;

    /**
     * Creates new instance of writeout-cr special form
     */
    public WriteOutCr() {
        super("writeout-cr", "&rest", "arguments");
    }

    @SuppressWarnings("UseOfSystemOutOrSystemErr")
    @Override
    public LispValue applyFunction(final Context context, final LispCollection parameters) {
        System.out.println(parameters.stream().map(LispValue::printIt).collect(Collectors.joining(" ")));
        return NilCollection.INSTANCE;
    }
}
