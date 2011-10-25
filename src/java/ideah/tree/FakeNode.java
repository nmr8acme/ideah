package ideah.tree;

import ideah.tree.decl.FunctionDecl;
import ideah.tree.expr.*;
import ideah.tree.pat.*;
import ideah.util.LineColRange;

import java.util.ArrayList;
import java.util.List;

public final class FakeNode extends Located {

    public final String name;
    public final List<FakeNode> children;

    public FakeNode(LineColRange location, String name, List<FakeNode> children) {
        super(location);
        this.name = name;
        this.children = children;
    }

    private void append(StringBuilder buf, String indent) {
        buf.append(indent + name + " " + location + " {\n");
        String newIndent = "    " + indent;
        for (FakeNode child : children) {
            child.append(buf, newIndent);
        }
        buf.append(indent + "}\n");
    }

    public String toString() {
        StringBuilder buf = new StringBuilder();
        append(buf, "");
        return buf.toString();
    }

    Located toStruct() {
        try {
            return toExpression();
        } catch (NoMatchException ex) {
            // ignore
        }
        try {
            return toPattern();
        } catch (NoMatchException ex) {
            // ignore
        }
        if ("ValD".equals(name)) {
            FakeNode child = children.get(0);
            Located struct = child.toStruct();
            if (struct != null) {
                return struct;
            } else {
                return this;
            }
        } else if ("FunBind".equals(name)) {
            List<Match> matches = new ArrayList<Match>();
            for (FakeNode child : children) {
                Match match = (Match) child.toStruct(); // todo
                matches.add(match);
            }
            return new FunctionDecl(location, null, matches); // todo: name
        } else if ("Match".equals(name)) {
            List<Pat> pats = new ArrayList<Pat>();
            List<GRHS> grhss = new ArrayList<GRHS>();
            for (FakeNode child : children) {
                try {
                    Pat pattern = child.toPattern();
                    pats.add(pattern);
                } catch (NoMatchException ex) {
                    Located struct = child.toStruct();
                    grhss.add((GRHS) struct); // todo
                }
            }
            return new Match(location, pats, new GRHSs(grhss, null)); // todo: where
        } else if ("GRHS".equals(name)) {
            Expression expression = null;
            for (FakeNode child : children) {
                Located struct = child.toStruct();
                if (struct instanceof Expression) {
                    expression = (Expression) struct; // todo
                }
            }
            // todo: statements
            return new GRHS(location, expression);
        } else {
            return this;
        }
    }

    private Expression toExpression() throws NoMatchException {
        if ("HsOverLit".equals(name)) {
            return new OverLiteral(location);
        } else if ("HsVar".equals(name)) {
            return new VarExpr(location);
        } else if ("HsLit".equals(name)) {
            return new Literal(location);
        } else if ("HsApp".equals(name)) {
            Expression function = children.get(0).toExpression();
            Expression arg = children.get(1).toExpression();
            return new Application(location, function, arg);
        } else if ("SectionL".equals(name)) {
            Expression arg = children.get(0).toExpression();
            Expression op = children.get(1).toExpression();
            return new LeftSection(location, arg, op);
        } else if ("SectionR".equals(name)) {
            Expression op = children.get(0).toExpression();
            Expression arg = children.get(1).toExpression();
            return new RightSection(location, op, arg);
        } else if ("HsPar".equals(name)) {
            Expression expression = children.get(0).toExpression();
            return new Parentheses(location, expression);
        } else if ("HsCase".equals(name)) {
            Expression expression = children.get(0).toExpression();
            List<Match> matches = new ArrayList<Match>();
            // todo: fill matches
            return new CaseExpr(location, expression, matches);
        } else if ("HsIf".equals(name)) {
            Expression expression = children.get(0).toExpression();
            Expression thenExpr = children.get(1).toExpression();
            Expression elseExpr = children.get(2).toExpression();
            return new IfExpr(location, expression, thenExpr, elseExpr);
        } else {
            // todo: other cases
            throw new NoMatchException();
        }
    }

    private Pat toPattern() throws NoMatchException {
        if ("WildPat".equals(name)) {
            return new WildPat(location);
        } else if ("VarPat".equals(name)) {
            return new VarPat(location, null); // todo: name
        } else if ("LitPat".equals(name) || "NPat".equals(name)) {
            return new LitPat(location);
        } else if ("ParPat".equals(name)) {
            Pat pattern = children.get(0).toPattern();
            return new ParenPat(location, pattern);
        } else if ("BangPat".equals(name)) {
            Pat pattern = children.get(0).toPattern();
            return new BangPat(location, pattern);
        } else if ("ListPat".equals(name)) {
            List<Pat> patterns = new ArrayList<Pat>();
            for (FakeNode child : children) {
                patterns.add(child.toPattern());
            }
            return new ListPat(location, patterns);
        } else if ("TuplePat".equals(name)) {
            List<Pat> patterns = new ArrayList<Pat>();
            for (FakeNode child : children) {
                patterns.add(child.toPattern());
            }
            return new TuplePat(location, patterns);
        } else {
            // todo: other cases
            throw new NoMatchException();
        }
    }
}
