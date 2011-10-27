package ideah.tree;

import ideah.util.LineColRange;

import java.util.Collections;

public final class Ident extends Located {

    public Ident(LineColRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
