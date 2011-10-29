package ideah.tree.stmt;

import ideah.tree.IRange;
import ideah.tree.Located;

public abstract class Statement extends Located {

    protected Statement(IRange location) {
        super(location);
    }
}
