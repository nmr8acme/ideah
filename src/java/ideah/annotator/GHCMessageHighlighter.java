package ideah.annotator;

import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.roots.ModuleFileIndex;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import ideah.HaskellFileType;
import ideah.compiler.GHCMessage;
import ideah.compiler.LaunchGHC;
import ideah.intentions.AutoImportIntention;
import ideah.sdk.HaskellSdkAdditionalData;
import ideah.util.*;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public final class GHCMessageHighlighter extends ExternalAnnotator<PsiFile, AnnotationResult> {

    private static final Logger LOG = Logger.getInstance("ideah.compiler.GHCMessageHighlighter");

    @Override
    public PsiFile collectInformation(@NotNull PsiFile file) {
        return file;
    }

    @Override
    public AnnotationResult doAnnotate(PsiFile psiFile) {
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null)
            return null;
        Module module = DeclarationPosition.getDeclModule(psiFile);
        if (module == null)
            return null;
        List<GHCMessage> ghcMessages = LaunchGHC.compile(null, file.getPath(), module, true);
        return new AnnotationResult(file, ghcMessages);
    }

    @Override
    public void apply(@NotNull PsiFile file, AnnotationResult result, @NotNull AnnotationHolder holder) {
        if (result == null)
            return;
        showMessages(file, holder, result.file, result.ghcMessages);
    }

    private static void showMessages(PsiFile psiFile, AnnotationHolder annotationHolder, VirtualFile file, List<GHCMessage> ghcMessages) {
        String path = file.getPath();
        File mainFile = new File(path);
        Map<String, SortedSet<String>> userImports = null;
        for (GHCMessage ghcMessage : ghcMessages) {
            if (FileUtil.filesEqual(new File(ghcMessage.getFileName()), mainFile)) {
                LineColRange lcRange = ghcMessage.getRange();
                TextRange range = lcRange.getRange(psiFile);
                String message = ghcMessage.getErrorMessage();
                CompilerMessageCategory category = ghcMessage.getCategory();

                Annotation out = null;
                switch (category) {
                case ERROR:
                    out = annotationHolder.createErrorAnnotation(range, message);
                    break;
                case WARNING:
                    out = annotationHolder.createWarningAnnotation(range, message);
                    break;
                case INFORMATION:
                    out = annotationHolder.createInfoAnnotation(range, message);
                    break;
                case STATISTICS:
                    break;
                }
                if (out != null) {
                    if (message.startsWith("Not in scope")) {
                        Module module = DeclarationPosition.getDeclModule(psiFile);
                        String symbol = psiFile.getText().substring(range.getStartOffset(), range.getEndOffset());
                        if (userImports == null) {
                            userImports = listUserImports(module);
                        }
                        List<String> imports = new ArrayList<String>();
                        ImportTrie importTrie = ImportTrie.get(module, path, getAllFiles(module));
                        addImports(imports, userImports, symbol, importTrie);
                        addStandardImports(module, symbol, imports, importTrie);
                        out.registerFix(new AutoImportIntention(psiFile, range, imports.toArray(new String[imports.size()]), symbol), range);
                    }
                    out.setTooltip(out.getTooltip().replaceAll("\\n", "<br/>").replaceAll("`(\\w+?)'", "<b>$1</b>"));
                }
            }
        }
    }

    private static List<String> getAllFiles(Module module) {
        return getAllFiles(module, null);
    }

    private static List<String> getAllFiles(Module module, final Path except) {
        ModuleFileIndex index = ModuleRootManager.getInstance(module).getFileIndex();
        final List<String> files = new ArrayList<String>();
        index.iterateContent(new ContentIterator() {
            public boolean processFile(VirtualFile fileOrDir) {
                if (!fileOrDir.isDirectory()) {
                    FileType fileType = fileOrDir.getFileType();
                    String path = fileOrDir.getPath();
                    if (HaskellFileType.INSTANCE.equals(fileType) && !Paths.get(path).equals(except)) { // todo equals works for path comparison?
                        files.add(path);
                    }
                }
                return true;
            }
        });
        return files;
    }

    private static Map<String, SortedSet<String>> listUserImports(Module module) {
        CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null)
            return Collections.emptyMap();
        final List<String> files = getAllFiles(module);
        List<String> args = compiler.getCompileOptionsList(
            "-m", "AutoImport",
            "-s", GHCUtil.rootsAsString(module, false)
        );
        args.addAll(files);
        try {
            ProcessLauncher launcher = new ProcessLauncher(false, null, args);
            String stdOut = launcher.getStdOut();
            return ParseAutoImports.parseAutoImports(stdOut);
        } catch (Exception ex) {
            LOG.error(ex);
        }
        return Collections.emptyMap();
    }

    private static void addStandardImports(Module module, String symbol, List<String> imports, ImportTrie importTrie) {
        Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null)
            return;
        SdkAdditionalData sdkAdditionalData = sdk.getSdkAdditionalData();
        if (!(sdkAdditionalData instanceof HaskellSdkAdditionalData))
            return;
        HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) sdkAdditionalData;
        Map<String, SortedSet<String>> autoImports = data.getAutoImports();
        addImports(imports, autoImports, symbol, importTrie);
    }

    private static void addImports(List<String> imports, Map<String, SortedSet<String>> autoImports, String symbol, ImportTrie importTrie) {
        if (autoImports == null)
            return;
        SortedSet<String> modules = autoImports.get(symbol);
        if (modules != null) {
            List<String> sortedModules = sortImportsByUsage(modules, importTrie);
            imports.addAll(sortedModules);
        }
    }

    private static List<String> sortImportsByUsage(SortedSet<String> modules, final ImportTrie importTrie) {
        List<String> moduleList = new ArrayList<String>(modules);
        Collections.sort(moduleList, new Comparator<String>() {
            @Override
            public int compare(String i1, String i2) {
                return Double.compare(importTrie.getScore(i2), importTrie.getScore(i1));
            }
        });
        return moduleList;
    }
}
