package ideah.tree;

import ideah.util.LineColRange;

public abstract class Located {

    protected final LineColRange location;

    protected Located(LineColRange location) {
        this.location = location;
    }
}
