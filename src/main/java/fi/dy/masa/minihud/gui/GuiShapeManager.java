package fi.dy.masa.minihud.gui;

import javax.annotation.Nullable;
import net.minecraft.client.gui.GuiScreen;
import fi.dy.masa.malilib.gui.BaseListScreen;
import fi.dy.masa.malilib.gui.widget.DropDownListWidget;
import fi.dy.masa.malilib.gui.widget.LabelWidget;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.GenericButton;
import fi.dy.masa.malilib.gui.widget.list.DataListWidget;
import fi.dy.masa.malilib.overlay.message.MessageDispatcher;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.gui.widget.WidgetShapeEntry;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.renderer.shapes.ShapeType;

public class GuiShapeManager extends BaseListScreen<DataListWidget<ShapeBase>>
{
    protected final DropDownListWidget<ShapeType> widgetDropDown;

    public GuiShapeManager()
    {
        super(10, 82, 20, 88, "minihud", ConfigScreen.ALL_TABS, ConfigScreen.SHAPES);

        this.setTitle("minihud.gui.title.shape_manager");

        // The position will get updated later
        this.widgetDropDown = new DropDownListWidget<>(160, 16, 200, 10, ShapeType.VALUES, ShapeType::getDisplayName);
        this.widgetDropDown.setZ((int) this.zLevel + 2);
    }

    @Override
    protected void initScreen()
    {
        super.initScreen();

        //this.clearWidgets();
        //this.createTabButtonContainerWidget();

        int x = 12;
        int y = 44;
        int rightX = this.x + this.screenWidth;
        int lw = StringUtils.getMaxStringRenderWidth(StringUtils.translate("minihud.gui.button.shapes.overlay_rendering"),
                                                     StringUtils.translate("minihud.gui.button.shapes.shape_renderer")) + 8;

        LabelWidget label = new LabelWidget(0xFFFFF040, "minihud.gui.button.shapes.overlay_rendering");
        label.setPosition(x, y + 5);
        this.addWidget(label);

        x += lw + 4;
        GenericButton button = new BooleanConfigButton(-1, 18, Configs.Generic.OVERLAYS_RENDERING_TOGGLE);
        button.setPosition(x, y);
        this.addWidget(button);

        this.widgetDropDown.setRight(rightX - 10);
        this.widgetDropDown.setY(y);
        this.addWidget(this.widgetDropDown);

        y += 18;
        x = 12;
        label = new LabelWidget(0xFFFFF040, "minihud.gui.button.shapes.shape_renderer");
        label.setPosition(x, y + 6);
        this.addWidget(label);

        x += lw + 4;
        button = new BooleanConfigButton(-1, 18, RendererToggle.SHAPE_RENDERER.getBooleanConfig());
        button.setPosition(x, y + 1);
        this.addWidget(button);

        button = new GenericButton("minihud.gui.button.add_shape");
        button.setRight(rightX - 10);
        button.setY(y);
        button.setActionListener(this::addShape);
        this.addWidget(button);
    }

    public void onSelectionChange(@Nullable ShapeBase entry)
    {
        ShapeBase old = ShapeManager.INSTANCE.getSelectedShape();
        ShapeManager.INSTANCE.setSelectedShape(old == entry ? null : entry);
    }

    protected void addShape()
    {
        ShapeType type = this.widgetDropDown.getSelectedEntry();

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

    @Override
    protected DataListWidget<ShapeBase> createListWidget(int listX, int listY, int listWidth, int listHeight)
    {
        DataListWidget<ShapeBase> listWidget = new DataListWidget<>(listX, listY, listWidth, listHeight, ShapeManager.INSTANCE::getAllShapes);
        listWidget.setEntryWidgetFactory(WidgetShapeEntry::new);
        listWidget.setFetchFromSupplierOnRefresh(true);
        listWidget.setAllowSelection(true);
        listWidget.getEntrySelectionHandler().setSelectedEntry(ShapeManager.INSTANCE.getSelectedShape());
        listWidget.getEntrySelectionHandler().setSelectionListener(this::onSelectionChange);

        return listWidget;
    }

    public static boolean screenValidator(@Nullable GuiScreen currentScreen)
    {
        return currentScreen instanceof GuiShapeManager;
    }

    public static GuiShapeManager openShapeManager(@Nullable GuiScreen currentScreen)
    {
        GuiShapeManager gui = new GuiShapeManager();
        gui.setCurrentTab(ConfigScreen.SHAPES);
        return gui;
    }
}
