package fi.dy.masa.minihud.gui.widget;

import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.util.GuiUtils;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.widget.button.OnOffButton;
import fi.dy.masa.malilib.gui.widget.list.entry.BaseDataListEntryWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.DataListEntryWidgetData;
import fi.dy.masa.malilib.render.text.StyledTextLine;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class WidgetShapeEntry extends BaseDataListEntryWidget<ShapeBase>
{
    private final ShapeBase shape;
    private final GenericButton configureButton;
    private final GenericButton toggleButton;
    private final GenericButton removeButton;
    private final int buttonsStartX;

    public WidgetShapeEntry(ShapeBase data, DataListEntryWidgetData constructData)
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
