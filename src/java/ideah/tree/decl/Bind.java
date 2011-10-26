package ideah.tree.decl;

import ideah.util.LineColRange;

public abstract class Bind extends Declaration {

    protected Bind(LineColRange location) {
        super(location);
    }
}
