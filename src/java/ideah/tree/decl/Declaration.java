package ideah.tree.decl;

import ideah.tree.Located;
import ideah.util.IRange;

public abstract class Declaration extends Located {

    protected Declaration(IRange location) {
        super(location);
    }
}
