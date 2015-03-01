package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.errors.UnexpectedValueException;
import net.ninjacat.semblance.errors.runtime.ParameterException;
import net.ninjacat.semblance.errors.runtime.ParameterValueExpectedException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.utils.Option;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Created on 28/02/15.
 */
public class Parameters implements Iterable<Parameter>, Serializable {
    private static final SymbolAtom OPTIONAL = symbol("&optional");
    private static final SymbolAtom REST = symbol("&rest");

    private final List<Parameter> formalParameters;
    private final Option<Parameter> restParameter;

    public Parameters(SList definitions) {
        formalParameters = new ArrayList<>();
        Sweeper sweeper = Sweeper.Normal;
        Parameter tempRest = null;
        for (LispValue value : definitions) {
            if (OPTIONAL.equals(value)) {
                sweeper = Sweeper.Optional;
            } else if (REST.equals(value)) {
                sweeper = Sweeper.Rest;
            } else {
                Parameter parameter = createParameter(value, sweeper);
                if (sweeper == Sweeper.Rest) {
                    tempRest = parameter;
                } else {
                    formalParameters.add(parameter);
                }
            }
        }
        if (tempRest == null && sweeper == Sweeper.Rest) {
            throw new ParameterException("&resp parameter not specified", definitions.getSourceInfo());
        }
        restParameter = Option.of(tempRest);
    }


    @Override
    public Iterator<Parameter> iterator() {
        return formalParameters.iterator();
    }

    public List<Parameter> getFormalParameters() {
        return formalParameters;
    }

    public Option<Parameter> getRestParameter() {
        return restParameter;
    }

    public void apply(Context context, SList actualParameters) {
        SList evaluated = context.evaluateList(actualParameters);
        applyList(context, evaluated);
    }

    private void applyList(Context context, SList evaluated) {
        SList params = evaluated;
        // assign all available parameters
        for (Parameter parameter : this) {
            if (params.isNil()) {
                if (parameter.isRequired()) {
                    throw new ParameterValueExpectedException(parameter.getName(), evaluated.getSourceInfo());
                } else {
                    parameter.setInContext(context, null);
                }
            } else {
                LispValue param = params.head();
                params = asSList(params.tail());
                parameter.setInContext(context, param);
            }
        }
        if (restParameter.isPresent()) {
            restParameter.get().setInContext(context, params);
        }
    }

    private Parameter createParameter(LispValue value, Sweeper sweeper) {
        switch (sweeper) {
            case Normal:
                return createNormalParameter(value);
            case Optional:
                return createOptionalParamter(value);
            case Rest:
                return createRestParameter(value);
        }
        throw new UnexpectedValueException(value);
    }

    private Parameter createRestParameter(LispValue value) {
        return new RestParameter(asSymbol(value));
    }

    private Parameter createNormalParameter(LispValue value) {
        return new StandardParameter(asSymbol(value));
    }

    private Parameter createOptionalParamter(LispValue value) {
        if (isSymbol(value)) {
            return new OptionalParameter(asSymbol(value),
                    Option.<LispValue>absent(), Option.<SymbolAtom>absent());
        } else if (isList(value)) {
            SList list = asSList(value);
            SymbolAtom name = asSymbol(list.head());
            LispValue defaultValue = list.tail().head();
            Option<SymbolAtom> flagName = list.length() > 2
                    ? Option.of(asSymbol(list.tail().tail().head()))
                    : Option.<SymbolAtom>absent();
            return new OptionalParameter(name, Option.of(defaultValue), flagName);
        } else {
            throw new UnexpectedValueException(value);
        }
    }

    private enum Sweeper {
        Normal,
        Optional,
        Rest
    }
}
