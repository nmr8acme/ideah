package ideah.tree.decl;

import ideah.tree.IRange;
import ideah.tree.Located;

public abstract class Declaration extends Located {

    protected Declaration(IRange location) {
        super(location);
    }
}
