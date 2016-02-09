package net.ninjacat.semblance.java;

public class ArrayPojo {

    public final int[] data;
    public String[] strData;
    public Object[] objData;

    public ArrayPojo() {
        this.data = new int[]{1, 2, 3};
    }

    public ArrayPojo(final int[] data) {
        this.data = data;
    }

    public int[] getData() {
        return data;
    }
}
