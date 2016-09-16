package net.ninjacat.semblance.java;

public class EnumPojo {

    public SEnum convert(final SEnum input) {
        if (input == SEnum.Value1) {
            return SEnum.Value2;
        } else {
            return SEnum.Value1;
        }
    }

    public SEnum get() {
        return SEnum.Value1;
    }
}
