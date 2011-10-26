package ideah.tree.stmt;

import ideah.tree.Located;
import ideah.util.LineColRange;

public abstract class Statement extends Located {

    protected Statement(LineColRange location) {
        super(location);
    }
}
