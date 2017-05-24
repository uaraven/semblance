package net.ninjacat.semblance.ui;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import net.ninjacat.semblance.Interpreter;

import java.net.URL;

public class SemblanceEdit extends Application {


    private Interpreter interpreter;

    @Override
    public void start(final Stage primaryStage) throws Exception {

        interpreter = new Interpreter();

        final URL location = getClass().getResource("editor.fxml");
        final FXMLLoader fxmlLoader = new FXMLLoader(location);

        final Pane root = (Pane) fxmlLoader.load();
        final EditorController controller = fxmlLoader.getController();

        controller.setInterpreter(this.interpreter);

        primaryStage.setTitle("Semblance");
        final Scene scene = new Scene(root, 800, 600);
        scene.getStylesheets().add("/net/ninjacat/semblance/ui/semblance.css");
        primaryStage.setScene(scene);
        primaryStage.show();
    }
}
