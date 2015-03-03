package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.builtin.spforms.PrintLn;
import net.ninjacat.semblance.builtin.spforms.Var;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.callables.SpecialForm;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Base context for every program.
 * <p/>
 * Created on 01/03/15.
 */
public class RootContext extends DefaultContext {
    public RootContext() {
        super("/", null);

        bind(symbol("nil"), NilCollection.INSTANCE);
        bind(symbol("T"), symbol("T"));
        bind(symbol("F"), symbol("F"));

        bindSpecialForms();
    }

    private void bindSpecialForms() {
        bindForm(new Var());
        bindForm(new PrintLn());
    }

    private void bindForm(SpecialForm form) {
        bind(form.name(), form);
    }
}