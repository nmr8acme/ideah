package ideah.tree;

import ideah.tree.decl.Declaration;

import java.util.List;

public final class Module extends Located {

    public final Ident name;
    public final List<Export> exports;
    public final List<Import> imports;
    public final List<Declaration> declarations;

    public Module(Ident name, List<Export> exports, List<Import> imports, List<Declaration> declarations) {
        this.name = name;
        this.exports = exports;
        this.imports = imports;
        this.declarations = declarations;
    }
}
