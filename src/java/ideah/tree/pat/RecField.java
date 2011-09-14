package ideah.tree.pat;

import ideah.tree.Ident;

public final class RecField {

    public final Ident name;
    public final Pat pattern;

    public RecField(Ident name, Pat pattern) {
        this.name = name;
        this.pattern = pattern;
    }
}
