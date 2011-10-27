package ideah.tree;

import ideah.util.LineColRange;

import java.util.Collections;

public final class Filler extends Located {

    public final String text;

    public Filler(LineColRange location, String text) {
        super(location);
        this.text = text;
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
