package ideah.tree;

import ideah.util.LineColRange;

import java.util.Collections;

public final class Filler extends Located {

    public Filler(LineColRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
