package net.ninjacat.semblance.ui;

import javafx.concurrent.Worker;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;
import net.ninjacat.semblance.java.Lambdas;
import netscape.javascript.JSObject;

import javax.annotation.Nonnull;
import java.net.URL;
import java.util.ResourceBundle;

import static java.util.stream.Collectors.joining;
import static net.ninjacat.semblance.utils.Values.string;
import static net.ninjacat.semblance.utils.Values.symbol;

public class EditorController implements Initializable {

    @FXML
    private WebView webView;
    @FXML
    private Button runButton;
    @FXML
    private TextFlow consoleOut;
    @FXML
    private TextField consoleIn;
    @FXML
    private Label position;
    @FXML
    private Label messageBox;
    @FXML
    private AnchorPane editorStatus;
    @FXML
    private ScrollPane consoleScroll;

    private Interpreter interpreter = null;

    @Override
    public void initialize(final URL location, final ResourceBundle resources) {
        final WebEngine webEngine = webView.getEngine();
        loadEditorTemplate();

        webView.autosize();
        consoleOut.autosize();
        editorStatus.autosize();
        consoleScroll.autosize();

        consoleScroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
        consoleScroll.setVbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED);

        consoleIn.prefWidthProperty().bind(consoleScroll.widthProperty().subtract(4));

        runButton.setOnAction(event -> runScript());

        webEngine.getLoadWorker().stateProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue == Worker.State.SUCCEEDED) {
                final JSObject window = (JSObject) webEngine.executeScript("window");
                final EditorAccess access = new EditorAccess();
                window.setMember("ui", access);
                final JSObject position = (JSObject) webEngine.executeScript("editor.getCursor()");
                access.positionChanged((int) position.getMember("line"), (int) position.getMember("ch"));
            }
        });

    }

    public void clearErrorLine() {
        webView.getEngine().executeScript("clearErrorHighlighting()");
    }

    public void setErrorLine(final int lineNumber) {
        webView.getEngine().executeScript("highlightError(" + lineNumber + ")");
    }

    public void setCursor(final int line, final int pos) {
        webView.getEngine().executeScript(String.format("editor.setCursor(%s, %s)", line, pos));
    }

    public String getText() {
        return webView.getEngine().executeScript("editor.getValue()").toString();
    }

    public LispValue runScript() {
        clearErrorLine();
        messageBox.setVisible(false);
        editorStatus.autosize();
        try {
            final String program = getText();
            return interpreter.run(program);
        } catch (final ParsingException e) {
            highlightError(e.getSourceInfo(), e.getMessage());
        } catch (final SemblanceRuntimeException re) {
            highlightError(re.getSourceInfo(), re.getMessage());
        }
        return NilCollection.INSTANCE;
    }

    public void setInterpreter(final Interpreter interpreter) {
        this.interpreter = interpreter;
        this.interpreter.injectContextModifier(new ConsoleContextModifier());
    }

    /**
     * applies the editing template to the editing code to create the html+javascript source for a code editor.
     */
    private void loadEditorTemplate() {
        final String url = getClass().getResource("/web/editor.html").toExternalForm();
        webView.getEngine().load(url);
    }

    private void highlightError(final SourceInfo sourceInfo, final String message) {
        if (!sourceInfo.equals(SourceInfo.UNKNOWN)) {
            setErrorLine(sourceInfo.getLine());
            setCursor(sourceInfo.getLine(), sourceInfo.getPosition());

            webView.requestFocus();
        }
        if (message != null && !message.trim().isEmpty()) {
            messageBox.setText(message);
            messageBox.setVisible(true);
        } else {
            messageBox.setVisible(false);
        }
        editorStatus.autosize();
    }

    public class EditorAccess {
        public void positionChanged(final int col, final int row) {
            position.setText(String.format("%d:%d", col + 1, row + 1));
        }
    }

    private class ConsoleContextModifier implements ContextModifier {

        @Override
        public void modify(@Nonnull final Context context) {
            context.bind(symbol("print"), Lambdas.methodAsFunction(this::print));
            context.bind(symbol("println"), Lambdas.methodAsFunction(this::println));
        }

        public LispValue print(final Context context, final LispCollection parameters) {
            final String outString = parameters.stream().map(LispValue::printIt).collect(joining(" "));
            return doPrint(outString);
        }

        public LispValue println(final Context context, final LispCollection parameters) {
            final String outString = parameters.stream().map(LispValue::printIt).collect(joining(" ")) + "\n";
            return doPrint(outString);
        }

        private LispValue doPrint(final String value) {
            final Text e = new Text(value);
            e.setStyle("-fx-text-fill: white");
            consoleOut.getChildren().add(e);
            return string(value);
        }
    }
}
