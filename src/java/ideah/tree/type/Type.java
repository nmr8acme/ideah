package ideah.tree.type;

import ideah.tree.Located;
import ideah.util.LineColRange;

public abstract class Type extends Located {

    protected Type(LineColRange location) {
        super(location);
    }
}
