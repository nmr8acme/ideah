package ideah.tree.decl;

import ideah.tree.Ident;
import ideah.tree.Located;
import ideah.tree.type.Type;
import ideah.util.LineColRange;

import java.util.List;

public final class ConDecl extends Located {

    public final Ident name;
    public final List<Type> types;

    public ConDecl(LineColRange location, Ident name, List<Type> types) {
        super(location);
        this.name = name;
        this.types = types;
    }
}
