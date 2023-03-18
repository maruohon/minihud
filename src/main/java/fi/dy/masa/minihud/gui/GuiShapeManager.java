package fi.dy.masa.minihud.gui;

import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.GuiListBase;
import fi.dy.masa.malilib.gui.Message.MessageType;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.ButtonOnOff;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.gui.interfaces.ISelectionListener;
import fi.dy.masa.malilib.gui.widgets.WidgetDropDownList;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.gui.GuiConfigs.ConfigGuiTab;
import fi.dy.masa.minihud.gui.widgets.WidgetListShapes;
import fi.dy.masa.minihud.gui.widgets.WidgetShapeEntry;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.renderer.shapes.ShapeType;

public class GuiShapeManager extends GuiListBase<ShapeBase, WidgetShapeEntry, WidgetListShapes>
                             implements ISelectionListener<ShapeBase>
{
    protected final WidgetDropDownList<ShapeType> widgetDropDown;
    protected final ButtonOnOff shapeRendererToggleButton;

    public GuiShapeManager()
    {
        super(10, 68);

        this.title = StringUtils.translate("minihud.gui.title.shape_manager");

        this.shapeRendererToggleButton = new ButtonOnOff(10, 42, -1, false, "minihud.gui.button.shape_renderer_toggle", RendererToggle.SHAPE_RENDERER.getBooleanValue());

        // The position will get updated later
        this.widgetDropDown = new WidgetDropDownList<>(0, 0, 160, 18, 200, 10, ImmutableList.copyOf(ShapeType.values()), ShapeType::getDisplayName);
        this.widgetDropDown.setZLevel(100);
    }

    @Override
    protected int getBrowserWidth()
    {
        return this.width - 20;
    }

    @Override
    protected int getBrowserHeight()
    {
        return this.height - this.getListY() - 6;
    }

    @Override
    public void initGui()
    {
        GuiConfigs.tab = ConfigGuiTab.SHAPES;

        super.initGui();

        this.clearWidgets();
        this.clearButtons();
        this.createTabButtons();
        this.getListWidget().refreshEntries();
    }

    protected void createTabButtons()
    {
        int x = 10;
        int y = 26;
        int rows = 1;

        for (ConfigGuiTab tab : ConfigGuiTab.values())
        {
            int width = this.getStringWidth(tab.getDisplayName()) + 10;

            if (x >= this.width - width - 10)
            {
                x = 10;
                y += 22;
                ++rows;
            }

            x += this.createTabButton(x, y, width, tab);
        }

        String name = StringUtils.translate("minihud.gui.button.add_shape");
        ButtonGeneric addShapeButton = new ButtonGeneric(this.width - 10, y, -1, true, name);

        // Check if there is enough space to put the dropdown widget and
        // the button at the end of the last tab button row
        if (rows < 2 || (this.width - 10 - x < (addShapeButton.getWidth() + this.widgetDropDown.getWidth() + 4)))
        {
            y += 22;
        }

        addShapeButton.setY(y);

        this.setListPosition(this.getListX(), y + 20);
        this.reCreateListWidget();

        this.shapeRendererToggleButton.setPosition(10, y + 1);
        this.addButton(this.shapeRendererToggleButton, (b, mb) -> this.toggleShapeRendererOnOff());

        this.widgetDropDown.setPosition(addShapeButton.getX() - this.widgetDropDown.getWidth() - 4, y + 1);

        this.addWidget(this.widgetDropDown);
        this.addButton(addShapeButton, (btn, mbtn) -> {
            ShapeType shape = this.widgetDropDown.getSelectedEntry();

            if (shape != null)
            {
                ShapeManager.INSTANCE.addShape(shape.createShape());
                this.getListWidget().refreshEntries();
            }
            else
            {
                InfoUtils.showGuiOrInGameMessage(MessageType.ERROR, "Select the shape from the dropdown");
            }
        });
    }

    protected void toggleShapeRendererOnOff()
    {
        RendererToggle.SHAPE_RENDERER.toggleBooleanValue();
        this.shapeRendererToggleButton.updateDisplayString(RendererToggle.SHAPE_RENDERER.getBooleanValue());
    }

    protected int createTabButton(int x, int y, int width, ConfigGuiTab tab)
    {
        ButtonGeneric button = new ButtonGeneric(x, y, width, 20, tab.getDisplayName());
        button.setEnabled(GuiConfigs.tab != tab);
        this.addButton(button, new ButtonListenerTab(tab));

        return button.getWidth() + 2;
    }

    @Override
    public void onSelectionChange(@Nullable ShapeBase entry)
    {
        ShapeBase old = ShapeManager.INSTANCE.getSelectedShape();
        ShapeManager.INSTANCE.setSelectedShape(old == entry ? null : entry);
    }

    @Override
    protected WidgetListShapes createListWidget(int listX, int listY)
    {
        return new WidgetListShapes(listX, listY, this.getBrowserWidth(), this.getBrowserHeight(), 0, this);
    }

    public static class ButtonListenerTab implements IButtonActionListener
    {
        private final ConfigGuiTab tab;

        public ButtonListenerTab(ConfigGuiTab tab)
        {
            this.tab = tab;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            GuiConfigs.tab = this.tab;
            GuiBase.openGui(new GuiConfigs());
        }
    }
}
