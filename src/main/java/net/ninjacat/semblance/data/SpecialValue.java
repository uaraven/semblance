package net.ninjacat.semblance.data;

/**
 * Created on 27/02/15.
 */
public class SpecialValue implements LispValue {

    public static final SpecialValue LIST_END = new SpecialValue();
    public static final SpecialValue PROGRAM_END = new SpecialValue();

    @Override
    public SemblanceType getType() {
        return SemblanceType.SPECIAL;
    }

    @Override
    public LispValue evaluate() {
        return this;
    }

    @Override
    public LispValue self() {
        return this;
    }

    @Override
    public String repr() {
        return "special";
    }
}
