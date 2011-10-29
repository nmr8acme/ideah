package ideah.tree.type;

import ideah.tree.IRange;
import ideah.tree.Located;

public abstract class Type extends Located {

    protected Type(IRange location) {
        super(location);
    }
}
