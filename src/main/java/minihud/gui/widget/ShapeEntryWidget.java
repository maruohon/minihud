package minihud.gui.widget;

import malilib.gui.BaseScreen;
import malilib.gui.util.GuiUtils;
import malilib.gui.widget.button.GenericButton;
import malilib.gui.widget.button.OnOffButton;
import malilib.gui.widget.list.entry.BaseDataListEntryWidget;
import malilib.gui.widget.list.entry.DataListEntryWidgetData;
import malilib.render.text.StyledTextLine;
import minihud.gui.GuiShapeEditor;
import minihud.renderer.shapes.ShapeBase;
import minihud.renderer.shapes.ShapeManager;

public class ShapeEntryWidget extends BaseDataListEntryWidget<ShapeBase>
{
    private final ShapeBase shape;
    private final GenericButton configureButton;
    private final GenericButton toggleButton;
    private final GenericButton removeButton;
    private final int buttonsStartX;

    public ShapeEntryWidget(ShapeBase data, DataListEntryWidgetData constructData)
    {
        super(data, constructData);

        this.shape = data;
        this.toggleButton = OnOffButton.simpleSlider(20, this.shape::isShapeEnabled, this::toggleShapeEnabled);

        this.configureButton = GenericButton.create("malilib.button.misc.configure");
        this.configureButton.setActionListener(() -> {
            GuiShapeEditor gui = new GuiShapeEditor(this.shape);
            gui.setParent(GuiUtils.getCurrentScreen());
            BaseScreen.openScreen(gui);
        });

        this.removeButton = GenericButton.create("malilib.button.misc.remove");
        this.removeButton.setActionListener(() -> {
            ShapeManager.INSTANCE.removeShape(this.shape);
            this.listWidget.refreshEntries();
        });

        this.buttonsStartX = this.getRight() - this.configureButton.getWidth() - this.toggleButton.getWidth() - this.removeButton.getWidth() - 6;

        this.setText(StyledTextLine.of(data.getDisplayName()));
        this.setHoverStringProvider("shape_info", data::getWidgetHoverLines);
        this.getBackgroundRenderer().getNormalSettings().setEnabled(true);
    }

    @Override
    public void reAddSubWidgets()
    {
        super.reAddSubWidgets();

        this.addWidget(this.configureButton);
        this.addWidget(this.toggleButton);
        this.addWidget(this.removeButton);
    }

    @Override
    public void updateSubWidgetPositions()
    {
        super.updateSubWidgetPositions();

        int y = this.getY() + 1;

        this.removeButton.setRight(this.getRight() - 4);
        this.removeButton.setY(y);

        this.toggleButton.setRight(this.removeButton.getX() - 2);
        this.toggleButton.setY(y);

        this.configureButton.setRight(this.toggleButton.getX() - 2);
        this.configureButton.setY(y);
    }

    @Override
    public boolean canHoverAt(int mouseX, int mouseY, int mouseButton)
    {
        return mouseX < this.buttonsStartX && super.canHoverAt(mouseX, mouseY, mouseButton);
    }

    @Override
    protected boolean isSelected()
    {
        return ShapeManager.INSTANCE.getSelectedShape() == this.data;
    }

    protected void toggleShapeEnabled()
    {
        this.shape.toggleEnabled();
        this.listWidget.reCreateListEntryWidgets();
    }
}
