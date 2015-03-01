package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.errors.runtime.ParameterException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;
import net.ninjacat.smooth.utils.Option;
import org.junit.Test;

import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;

public class ParametersTest {

    @Test
    public void shouldParseStandardParameters() throws Exception {
        Parameters parameters = new Parameters(list(symbol("param1"), symbol("param2")));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (StandardParameter) params.get(0), is(new StandardParameter(symbol("param1"))));
        assertThat("Should parse standard params",
                (StandardParameter) params.get(1), is(new StandardParameter(symbol("param2"))));
    }

    @Test
    public void shouldParseOptionalParamNoDefaults() throws Exception {
        Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                symbol("opt1")));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (StandardParameter) params.get(0), is(new StandardParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Option.<LispValue>absent(), Option.<SymbolAtom>absent())));
    }


    @Test
    public void shouldParseOptionalParamWithDefault() throws Exception {
        Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                list(symbol("opt1"), number(10L))
        ));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (StandardParameter) params.get(0), is(new StandardParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Option.of(number(10L)), Option.<SymbolAtom>absent())));
    }

    @Test
    public void shouldParseOptionalParamWithDefaultAndFlag() throws Exception {
        Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                list(symbol("opt1"), number(10L), symbol("opt1-supplied-p"))
        ));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (StandardParameter) params.get(0), is(new StandardParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Option.of(number(10L)), Option.of(symbol("opt1-supplied-p")))));
    }

    @Test
    public void shouldParseRestParameter() throws Exception {
        Parameters parameters = new Parameters(list(symbol("&rest"), symbol("rest1")));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse &rest params",
                parameters.getRestParameter().isPresent(), is(true));
    }

    @Test
    public void shouldParseStandardAndRestParameter() throws Exception {
        Parameters parameters = new Parameters(list(symbol("param1"), symbol("&rest"), symbol("rest1")));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (StandardParameter) params.get(0), is(new StandardParameter(symbol("param1"))));
        assertThat("Should parse &rest params",
                parameters.getRestParameter().isPresent(), is(true));
    }

    @Test
    public void shouldParseStandardOptionalAndRestParameter() throws Exception {
        Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                list(symbol("opt1"), number(10L), symbol("opt1-supplied-p")),
                symbol("&rest"), symbol("rest1")));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (StandardParameter) params.get(0), is(new StandardParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Option.of(number(10L)), Option.of(symbol("opt1-supplied-p")))));
        assertThat("Should parse &rest params",
                parameters.getRestParameter().isPresent(), is(true));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailWhenParamListContainsString() throws Exception {
        Parameters parameters = new Parameters(list(string("param1")));

        parameters.getFormalParameters();
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailWhenParamListContainsList() throws Exception {
        Parameters parameters = new Parameters(list(list(string("param1"))));

        parameters.getFormalParameters();
    }

    @Test
    public void shouldBindActualParameter() throws Exception {
        SymbolAtom param = symbol("param1");
        Parameters parameters = new Parameters(list(param));
        Context context = mock(Context.class);
        when(context.evaluateList(list(number(10)))).thenReturn(list(number(10)));

        parameters.apply(context, list(number(10)));

        verify(context).bind(param, number(10));
    }

    @Test
    public void shouldBindMultipleActualParameters() throws Exception {
        SymbolAtom param1 = symbol("param1");
        SymbolAtom param2 = symbol("param2");
        Parameters parameters = new Parameters(list(param1, param2));
        Context context = mock(Context.class);
        SList actualParams = list(number(10), string("v"));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
    }

    @Test
    public void shouldBindOptionalParameter() throws Exception {
        SymbolAtom param1 = symbol("param1");
        SymbolAtom param2 = symbol("param2");
        Parameters parameters = new Parameters(list(param1, symbol("&optional"), param2));
        Context context = mock(Context.class);
        SList actualParams = list(number(10), string("v"));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
    }

    @Test
    public void shouldBindNilAsOmittedOptionalParameter() throws Exception {
        SymbolAtom param1 = symbol("param1");
        SymbolAtom param2 = symbol("param2");
        Parameters parameters = new Parameters(list(param1, symbol("&optional"), param2));
        Context context = mock(Context.class);
        SList actualParams = list(number(10));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, NilCollection.INSTANCE);
    }


    @Test
    public void shouldBindNilAsOmittedOptionalParameterWithDefault() throws Exception {
        SymbolAtom param1 = symbol("param1");
        SymbolAtom param2 = symbol("param2");
        Parameters parameters = new Parameters(list(param1, symbol("&optional"), list(param2, string("v"))));
        Context context = mock(Context.class);
        SList actualParams = list(number(10));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);
        when(context.evaluate(string("v"))).thenReturn(string("v"));

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
    }

    @Test
    public void shouldBindNilAsOmittedOptionalParameterWithDefaultAndFlag() throws Exception {
        SymbolAtom param1 = symbol("param1");
        SymbolAtom param2 = symbol("param2");
        Parameters parameters = new Parameters(list(param1, symbol("&optional"), list(param2, string("v"), symbol("flag"))));
        Context context = mock(Context.class);
        SList actualParams = list(number(10));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);
        when(context.evaluate(string("v"))).thenReturn(string("v"));

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
        verify(context).bind(symbol("flag"), Values.T);
    }


    @Test(expected = ParameterException.class)
    public void shouldFailWhenRestParameterIsNotSpecified() throws Exception {
        SymbolAtom param = symbol("&rest");
        Parameters parameters = new Parameters(list(param));
        Context context = mock(Context.class);
        SList actualParameters = list(number(10), number(12));
        when(context.evaluateList(actualParameters)).thenReturn(actualParameters);

        parameters.apply(context, actualParameters);
    }


    @Test
    public void shouldBindRestParameter() throws Exception {
        SymbolAtom rest = symbol("&rest");
        SymbolAtom rest_param = symbol("rest_param");
        Parameters parameters = new Parameters(list(rest, rest_param));
        Context context = mock(Context.class);
        SList actualParameters = list(number(10), number(12));
        when(context.evaluateList(actualParameters)).thenReturn(actualParameters);

        parameters.apply(context, actualParameters);

        verify(context).bind(rest_param, actualParameters);
    }



    @Test
    public void shouldBindEmptyRestParameter() throws Exception {
        SymbolAtom param = symbol("param");
        SymbolAtom rest = symbol("&rest");
        SymbolAtom rest_param = symbol("rest_param");
        Parameters parameters = new Parameters(list(param, rest, rest_param));
        Context context = mock(Context.class);
        SList actualParameters = list(number(10));
        when(context.evaluateList(actualParameters)).thenReturn(actualParameters);

        parameters.apply(context, actualParameters);

        verify(context).bind(param, number(10));
        verify(context).bind(rest_param, NilCollection.INSTANCE);
    }


}
