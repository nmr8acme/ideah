package ideah.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.util.IncorrectOperationException;
import ideah.lexer.HaskellTokenTypes;
import ideah.lexer.LexedIdentifier;
import ideah.parser.HaskellElementTypes;
import ideah.psi.api.util.HaskellPsiElementFactory;
import ideah.util.DeclarationPosition;
import ideah.util.LineCol;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class HaskellAbstractIdentImpl extends HaskellBaseElementImpl implements PsiReference {

    private static final Logger LOG = Logger.getInstance("ideah.psi.impl.HaskellAbstractIdentImpl");

    protected HaskellAbstractIdentImpl(@NotNull ASTNode node) {
        super(node);
    }

    @Override
    public PsiReference getReference() {
        return this;
    }

    @Nullable
    public PsiElement setName(@NotNull @NonNls String name) throws IncorrectOperationException {
        LexedIdentifier parsedNewName = LexedIdentifier.parse(name);
        LexedIdentifier parsedOldName = LexedIdentifier.parseMaybeInfixPrefix(getText());
        LOG.assertTrue(!(parsedNewName == null || parsedOldName == null));
        boolean newNameOperator = HaskellTokenTypes.OPERATORS.contains(parsedNewName.type);
        String createdNewName;

        String module = parsedOldName.module;
        String nameWithModule = module == null ? name : module + "." + name;

        boolean prefixInfixIdent = isPrefixInfixIdent();
        boolean oldOrNewOperatorAndOneOfThemNot = !newNameOperator == HaskellTokenTypes.OPERATORS.contains(parsedOldName.type);

        if (oldOrNewOperatorAndOneOfThemNot != prefixInfixIdent) {
            if (newNameOperator) {
                createdNewName = "(" + nameWithModule + ")";
            } else {
                createdNewName = "`" + nameWithModule + "`";
            }
        } else {
            createdNewName = nameWithModule;
        }

        HaskellPsiElementFactory factory = HaskellPsiElementFactory.getInstance(getProject());
        ASTNode newIdentNode = factory.createIdentNodeFromText(createdNewName);
        if (newIdentNode == null)
            return this;
        ASTNode nodeToBeInsertedTo = getNodeToBeInsertedTo();
        ASTNode nodeToBeReplaced = getNodeToBeReplaced();
        nodeToBeInsertedTo.replaceChild(nodeToBeReplaced, newIdentNode);
        return newIdentNode.getPsi();
    }

    protected abstract ASTNode getNodeToBeReplaced();

    protected abstract ASTNode getNodeToBeInsertedTo();

    protected abstract boolean isPrefixInfixIdent();

    public PsiElement getElement() {
        return this;
    }

    public TextRange getRangeInElement() {
        return new TextRange(0, getTextLength());
    }

    public PsiElement resolve() {
        PsiFile psiFile = getContainingFile();
        LineCol coord = LineCol.fromOffset(psiFile, getTextOffset());
        try {
            DeclarationPosition declaration = DeclarationPosition.get(psiFile, coord);
            return getElementAt(getProject(), declaration);
        } catch (ProcessCanceledException ex) {
            return null;
        } catch (Exception e) {
            LOG.error(e);
            return null;
        }
    }

    public static PsiElement getElementAt(Project project, DeclarationPosition declaration) {
        if (declaration == null)
            return null;
        VirtualFile baseDir = project.getBaseDir();
        if (baseDir == null)
            return null;
        VirtualFile declarationModuleVirtualFile = baseDir.getFileSystem().findFileByPath(declaration.module);
        if (declarationModuleVirtualFile == null)
            return null;
        PsiFile declarationModulePsiFile = PsiManager.getInstance(project).findFile(declarationModuleVirtualFile);
        if (declarationModulePsiFile == null)
            return null;
        int declarationStart = declaration.coord.getOffset(declarationModulePsiFile);
        PsiElement elementAt = declarationModulePsiFile.findElementAt(declarationStart);
        if (elementAt == null)
            return null;
        ASTNode parentNode = elementAt.getParent().getNode();
        if (parentNode.getElementType() == HaskellElementTypes.INFIX_PREFIX_IDENT) {
            return new HPInfixPrefixIdentImpl(parentNode);
        }
        return new HPIdentImpl(elementAt.getNode());
    }

    @NotNull
    public String getCanonicalText() {
        return getText(); // todo: resolve full module name?
    }

    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        return setName(newElementName);
    }

    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        return null; // todo
    }

    public boolean isReferenceTo(PsiElement element) {
        return false; // todo
    }

    @NotNull
    public Object[] getVariants() {
        return new Object[0]; // todo
    }

    public boolean isSoft() {
        return false;
    }
}
