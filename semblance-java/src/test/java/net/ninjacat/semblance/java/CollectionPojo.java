package net.ninjacat.semblance.java;

import java.util.List;

@SuppressWarnings("ClassNamingConvention")
public class CollectionPojo {

    private final List<Integer> data;

    public CollectionPojo(final List<Integer> data) {
        this.data = data;
    }

    public List<Integer> getData() {
        return data;
    }
}
