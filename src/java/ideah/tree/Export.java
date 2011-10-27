package ideah.tree;

import ideah.util.LineColRange;

import java.util.Collections;

public final class Export extends Located {

    // todo

    public Export(LineColRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList(); // todo
    }
}
