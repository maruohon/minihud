package minihud.gui;

import javax.annotation.Nullable;

import net.minecraft.client.gui.GuiScreen;

import malilib.gui.BaseListScreen;
import malilib.gui.BaseScreen;
import malilib.gui.widget.DropDownListWidget;
import malilib.gui.widget.LabelWidget;
import malilib.gui.widget.button.BooleanConfigButton;
import malilib.gui.widget.button.GenericButton;
import malilib.gui.widget.list.DataListWidget;
import malilib.input.ActionResult;
import malilib.overlay.message.MessageDispatcher;
import minihud.Reference;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.gui.widget.ShapeEntryWidget;
import minihud.renderer.shapes.ShapeBase;
import minihud.renderer.shapes.ShapeManager;
import minihud.renderer.shapes.ShapeType;

public class ShapeManagerScreen extends BaseListScreen<DataListWidget<ShapeBase>>
{
    protected final DropDownListWidget<ShapeType> shapeTypeDropDown;
    protected final GenericButton addShapeButton;
    protected final GenericButton overlaysToggleButton;
    protected final GenericButton shapeRendererToggleButton;
    protected final LabelWidget overlaysLabel;
    protected final LabelWidget shapeRendererLabel;

    public ShapeManagerScreen()
    {
        super(10, 82, 20, 88, "minihud", ConfigScreen.ALL_TABS, ConfigScreen.SHAPES);

        this.overlaysLabel = new LabelWidget(0xFFFFF040, "minihud.label.shapes.overlay_rendering");
        this.shapeRendererLabel = new LabelWidget(0xFFFFF040, "minihud.label.shapes.shape_renderer");

        this.addShapeButton = GenericButton.create("minihud.button.shapes.add_shape", this::addShape);
        this.overlaysToggleButton = new BooleanConfigButton(-1, 16, Configs.Generic.OVERLAYS_RENDERING_TOGGLE);
        this.shapeRendererToggleButton = new BooleanConfigButton(-1, 16, RendererToggle.SHAPE_RENDERER.getBooleanConfig());

        this.shapeTypeDropDown = new DropDownListWidget<>(16, 12, ShapeType.VALUES, ShapeType::getDisplayName);

        this.setTitle("minihud.title.screen.shape_manager", Reference.MOD_VERSION);
        this.createSwitchModConfigScreenDropDown(Reference.MOD_INFO);
    }

    @Override
    protected void reAddActiveWidgets()
    {
        super.reAddActiveWidgets();

        this.addWidget(this.overlaysLabel);
        this.addWidget(this.shapeRendererLabel);

        this.addWidget(this.addShapeButton);
        this.addWidget(this.overlaysToggleButton);
        this.addWidget(this.shapeRendererToggleButton);

        this.addWidget(this.shapeTypeDropDown);
    }

    @Override
    protected void updateWidgetPositions()
    {
        super.updateWidgetPositions();

        int x = this.x + 12;
        int y = this.y + 45;
        this.overlaysLabel.setPosition(x, y + 4);
        this.shapeRendererLabel.setPosition(x, y + 22);

        x = Math.max(this.overlaysLabel.getRight(), this.shapeRendererLabel.getRight()) + 12;
        this.overlaysToggleButton.setPosition(x, y);
        this.shapeRendererToggleButton.setPosition(x, y + 18);

        x = this.getRight() - 10;
        this.shapeTypeDropDown.setRight(x);
        this.shapeTypeDropDown.setY(y);
        this.addShapeButton.setRight(x);
        this.addShapeButton.setY(y + 18);
    }

    @Override
    protected DataListWidget<ShapeBase> createListWidget()
    {
        DataListWidget<ShapeBase> listWidget = new DataListWidget<>(ShapeManager.INSTANCE::getAllShapes, true);

        listWidget.setAllowSelection(true);
        listWidget.getEntrySelectionHandler().setSelectedEntry(ShapeManager.INSTANCE.getSelectedShape());
        listWidget.getEntrySelectionHandler().setSelectionListener(this::onSelectionChange);
        listWidget.setDataListEntryWidgetFactory(ShapeEntryWidget::new);

        return listWidget;
    }

    public void onSelectionChange(@Nullable ShapeBase entry)
    {
        ShapeBase old = ShapeManager.INSTANCE.getSelectedShape();
        ShapeManager.INSTANCE.setSelectedShape(old == entry ? null : entry);
    }

    protected void addShape()
    {
        ShapeType type = this.shapeTypeDropDown.getSelectedEntry();

        if (type != null)
        {
            ShapeManager.INSTANCE.addShape(type.createShape());
            this.getListWidget().refreshEntries();
        }
        else
        {
            MessageDispatcher.error("minihud.message.error.shapes.select_shape_from_dropdown");
        }
    }

    public static boolean screenValidator(@Nullable GuiScreen currentScreen)
    {
        return currentScreen instanceof ShapeManagerScreen || currentScreen instanceof GuiShapeEditor;
    }

    public static ShapeManagerScreen openShapeManagerScreen()
    {
        ShapeManagerScreen gui = new ShapeManagerScreen();
        gui.setCurrentTab(ConfigScreen.SHAPES);
        return gui;
    }

    public static ActionResult openShapeManager()
    {
        BaseScreen.openScreen(openShapeManagerScreen());
        return ActionResult.SUCCESS;
    }
}
