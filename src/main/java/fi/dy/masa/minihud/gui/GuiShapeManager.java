package fi.dy.masa.minihud.gui;

import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.GuiListBase;
import fi.dy.masa.malilib.gui.Message.MessageType;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.gui.interfaces.ISelectionListener;
import fi.dy.masa.malilib.gui.widgets.WidgetDropDownList;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.StringUtils;
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

    public GuiShapeManager()
    {
        super(10, 64);

        this.title = StringUtils.translate("minihud.gui.title.shape_manager");

        // The position will get updated later
        this.widgetDropDown = new WidgetDropDownList<ShapeType>(0, 0, 160, 18, 200, 10, ImmutableList.copyOf(ShapeType.values()), (type) -> type.getDisplayName());
        this.widgetDropDown.setZLevel(this.getZOffset() + 100);
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
                rows++;
            }

            x += this.createTabButton(x, y, width, tab);
        }

        this.setListPosition(this.getListX(), 68 + (rows - 1) * 22);
        this.getListWidget().setSize(this.getBrowserWidth(), this.getBrowserHeight());
        this.getListWidget().initGui();

        y += 24;

        x = this.width - 10;
        x -= this.addButton(x, y, ButtonListener.Type.ADD_SHAPE);

        this.widgetDropDown.setPosition(x - this.widgetDropDown.getWidth() - 4, y + 1);

        this.addWidget(this.widgetDropDown);
    }

    protected int createTabButton(int x, int y, int width, ConfigGuiTab tab)
    {
        ButtonGeneric button = new ButtonGeneric(x, y, width, 20, tab.getDisplayName());
        button.setEnabled(GuiConfigs.tab != tab);
        this.addButton(button, new ButtonListenerTab(tab));

        return button.getWidth() + 2;
    }

    protected int addButton(int x, int y, ButtonListener.Type type)
    {
        ButtonGeneric button = new ButtonGeneric(x, y, -1, true, type.getDisplayName());
        this.addButton(button, new ButtonListener(ButtonListener.Type.ADD_SHAPE, this));
        return button.getWidth();
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
        return new WidgetListShapes(listX, listY, this.getBrowserWidth(), this.getBrowserHeight(), this.getZOffset(), this);
    }

    private static class ButtonListener implements IButtonActionListener
    {
        private final Type type;
        private final GuiShapeManager gui;

        public ButtonListener(Type type, GuiShapeManager gui)
        {
            this.type = type;
            this.gui = gui;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            if (this.type == Type.ADD_SHAPE)
            {
                ShapeType shape = this.gui.widgetDropDown.getSelectedEntry();

                if (shape != null)
                {
                    ShapeManager.INSTANCE.addShape(shape.createShape());
                    this.gui.getListWidget().refreshEntries();
                }
                else
                {
                    InfoUtils.showGuiOrInGameMessage(MessageType.ERROR, "Select the shape from the dropdown");
                }
            }
        }

        public enum Type
        {
            ADD_SHAPE   ("minihud.gui.button.add_shape");

            private final String translationKey;

            private Type(String translationKey)
            {
                this.translationKey = translationKey;
            }

            public String getDisplayName()
            {
                return StringUtils.translate(this.translationKey);
            }
        }
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
