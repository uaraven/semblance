package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.errors.TypeMismatchException;
import net.ninjacat.smooth.utils.Option;
import org.junit.Test;

import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

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
                (RestParameter) params.get(0), is(new RestParameter()));
    }

    @Test
    public void shouldParseStandardAndRestParameter() throws Exception {
        Parameters parameters = new Parameters(list(symbol("param1"), symbol("&rest"), symbol("rest1")));

        List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (StandardParameter) params.get(0), is(new StandardParameter(symbol("param1"))));
        assertThat("Should parse &rest params",
                (RestParameter) params.get(1), is(new RestParameter()));
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
                (RestParameter) params.get(2), is(new RestParameter()));
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
}
