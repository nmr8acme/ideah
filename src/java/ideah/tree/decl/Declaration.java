package ideah.tree.decl;

import ideah.tree.Located;
import ideah.util.LineColRange;

public abstract class Declaration extends Located {

    protected Declaration(LineColRange location) {
        super(location);
    }
}
