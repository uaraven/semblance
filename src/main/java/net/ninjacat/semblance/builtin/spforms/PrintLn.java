package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.DefaultContext;

import static net.ninjacat.semblance.utils.Values.list;
import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * (println val1 val2 val3)
 * <p/>
 * prints val1 val2 val3 and returns NIL
 * <p/>
 * Created on 01/03/15.
 */
public class PrintLn extends SpecialForm {

    public PrintLn() {
        super(list(symbol("println"), symbol("&rest"), symbol("arguments")));
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        Context localContext = DefaultContext.namelessChildContext(context);
        LispCollection evaluated = localContext.evaluateList(parameters);

        for (LispValue value : evaluated) {
            System.out.print(value.repr());
            System.out.print(" ");
        }
        System.out.println();
        return NilCollection.INSTANCE;
    }
}
