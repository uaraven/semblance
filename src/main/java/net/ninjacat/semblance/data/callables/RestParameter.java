package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;

/**
 * Created on 28/02/15.
 */
public class RestParameter extends BaseParameter {
    public RestParameter() {
        super(Values.symbol("&rest"));
    }

    @Override
    public void setInContext(Context context, LispValue actualValue) {
        context.bind(getName(), actualValue);
    }
}
