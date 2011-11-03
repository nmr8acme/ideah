package ideah.findUsages;

import com.intellij.openapi.application.QueryExecutorBase;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import ideah.HaskellFileType;
import ideah.HaskellFileTypeLoader;
import ideah.compiler.HaskellCompiler;
import ideah.psi.impl.HPIdentImpl;
import ideah.util.CompilerLocation;
import ideah.util.DeclarationPosition;
import ideah.util.LineCol;
import ideah.util.ProcessLauncher;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.File;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class HaskellReferencesSearch extends QueryExecutorBase<PsiReference, ReferencesSearch.SearchParameters> {

    private static final Logger LOG = Logger.getInstance("ideah.findUsages.HaskellReferencesSearch");

    public HaskellReferencesSearch() {
        super(true);
    }

    @Override
    public void processQuery(@NotNull ReferencesSearch.SearchParameters queryParameters, @NotNull Processor<PsiReference> consumer) {
        PsiElement element = queryParameters.getElementToSearch();
        if (element instanceof HPIdentImpl) {
            HPIdentImpl ident = (HPIdentImpl) element;
            PsiFile file = element.getContainingFile();
            try {
                DeclarationPosition declaration = DeclarationPosition.get(file, LineCol.fromOffset(file, element.getTextOffset()));
                LineCol coord = declaration.coord;
                VirtualFile virtualFile = file.getVirtualFile();
                final Project project = file.getProject();
                ProjectFileIndex fileIndex = ProjectRootManager.getInstance(project).getFileIndex();
                Module module = fileIndex.getModuleForFile(virtualFile);
                CompilerLocation compiler = CompilerLocation.get(module);
                List<String> args = new ArrayList<String>();
                args.addAll(Arrays.asList(compiler.exe,
                    "-m", "FindUsages",
                    "-g", compiler.libPath,
                    "-s", CompilerLocation.rootsAsString(module, false),
                    "--line-number", String.valueOf(coord.line), "--column-number", String.valueOf(coord.column),
                    virtualFile.getPath()));
                final List<String> srcFiles = new ArrayList<String>();
                fileIndex.iterateContent(new ContentIterator() {
                    public boolean processFile(VirtualFile virtualFile) {
                        boolean isCompilableFile = HaskellCompiler.isCompilableFile(virtualFile);
                        if (isCompilableFile) {
                            srcFiles.add(virtualFile.getPath());
                        }
                        return isCompilableFile;
                    }
                });
                ProcessLauncher launcher = new ProcessLauncher(true, null, srcFiles);
                BufferedReader bf = new BufferedReader(new StringReader(launcher.getStdOut()));
                String line = bf.readLine();
                while (line != null) {
                    LineCol refLineCol = LineCol.parse(line);
                    line = bf.readLine();
                    if (line != null) {
                        PsiElement elementAt = HPIdentImpl.getElementAt(project, new DeclarationPosition(refLineCol, LineCol.cleanString(line)));
                        PsiReference reference = elementAt.getReference();
                        consumer.process(reference);
                    }
                }
            } catch (Exception e) {
                LOG.error(e);
            }
        }
    }
}
