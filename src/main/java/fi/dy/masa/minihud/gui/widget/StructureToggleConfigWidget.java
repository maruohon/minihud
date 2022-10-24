package fi.dy.masa.minihud.gui.widget;

import it.unimi.dsi.fastutil.ints.IntArrayList;

import malilib.gui.config.ConfigWidgetContext;
import malilib.gui.widget.ColorIndicatorWidget;
import malilib.gui.widget.KeybindSettingsWidget;
import malilib.gui.widget.button.BooleanConfigButton;
import malilib.gui.widget.button.KeyBindConfigButton;
import malilib.gui.widget.list.entry.DataListEntryWidgetData;
import malilib.gui.widget.list.entry.config.BaseConfigWidget;
import fi.dy.masa.minihud.config.StructureToggle;

public class StructureToggleConfigWidget extends BaseConfigWidget<StructureToggle>
{
    protected final StructureToggle config;
    protected final IntArrayList initialHotkeyValue = new IntArrayList();
    protected final BooleanConfigButton booleanButton;
    protected final KeyBindConfigButton hotkeyButton;
    protected final KeybindSettingsWidget settingsWidget;
    protected final ColorIndicatorWidget colorIndicatorWidgetMain;
    protected final ColorIndicatorWidget colorIndicatorWidgetComponents;
    protected final boolean initialBooleanValue;
    protected final int initialMainColor;
    protected final int initialComponentColor;

    public StructureToggleConfigWidget(StructureToggle config,
                                       DataListEntryWidgetData constructData,
                                       ConfigWidgetContext ctx)
    {
        super(config, constructData, ctx);

        this.config = config;
        this.initialBooleanValue = config.isEnabled();
        this.initialMainColor = config.getColorMain().getIntegerValue();
        this.initialComponentColor = config.getColorComponents().getIntegerValue();
        this.config.getKeyBind().getKeysToList(this.initialHotkeyValue);

        this.booleanButton = new BooleanConfigButton(-1, 20, config.getBooleanConfig());
        this.booleanButton.setActionListener(() -> {
            this.config.getBooleanConfig().toggleBooleanValue();
            this.updateWidgetState();
        });

        this.hotkeyButton = new KeyBindConfigButton(120, 20, config.getKeyBind(), ctx.getKeybindEditingScreen());
        this.hotkeyButton.setValueChangeListener(this::updateWidgetState);

        this.settingsWidget = new KeybindSettingsWidget(config.getKeyBind(), config.getDisplayName());

        this.colorIndicatorWidgetMain = new ColorIndicatorWidget(18, 18, this.config.getColorMain(), (newValue) -> {
            this.config.getColorMain().setValueFromInt(newValue);
            this.updateWidgetState();
        });
        this.colorIndicatorWidgetMain.getHoverInfoFactory().translateAndAddString(90, "minihud.hover.structures.color_main");

        this.colorIndicatorWidgetComponents = new ColorIndicatorWidget(18, 18, this.config.getColorComponents(), (newValue) -> {
            this.config.getColorComponents().setValueFromInt(newValue);
            this.updateWidgetState();
        });
        this.colorIndicatorWidgetComponents.getHoverInfoFactory().translateAndAddString(90, "minihud.hover.structures.color_components");
    }

    @Override
    public void reAddSubWidgets()
    {
        super.reAddSubWidgets();

        this.addWidget(this.booleanButton);
        this.addWidget(this.hotkeyButton);
        this.addWidget(this.settingsWidget);
        this.addWidget(this.colorIndicatorWidgetMain);
        this.addWidget(this.colorIndicatorWidgetComponents);
        this.addWidget(this.resetButton);
    }

    @Override
    public void updateSubWidgetPositions()
    {
        super.updateSubWidgetPositions();

        int x = this.getElementsStartPosition();
        int y = this.getY() + 1;
        int w = this.getElementWidth() - this.booleanButton.getWidth() - 66;

        this.booleanButton.setPosition(x, y);
        this.hotkeyButton.setWidth(w);
        this.hotkeyButton.setPosition(this.booleanButton.getRight() + 2, y);
        this.settingsWidget.setPosition(this.hotkeyButton.getRight() + 2, y);
        this.colorIndicatorWidgetMain.setPosition(this.settingsWidget.getRight() + 3, y + 1);
        this.colorIndicatorWidgetComponents.setPosition(this.colorIndicatorWidgetMain.getRight() + 3, y + 1);

        this.resetButton.setPosition(this.colorIndicatorWidgetComponents.getRight() + 4, y);
    }

    @Override
    public void updateWidgetState()
    {
        super.updateWidgetState();

        this.booleanButton.setEnabled(this.config.getBooleanConfig().isLocked() == false);
        this.booleanButton.updateButtonState();
        this.hotkeyButton.updateButtonState();
    }

    @Override
    protected boolean isResetEnabled()
    {
        return this.config.isModified() && this.config.getBooleanConfig().isLocked() == false;
    }

    @Override
    public boolean wasModified()
    {
        return this.config.isEnabled() != this.initialBooleanValue ||
               this.config.getColorMain().getIntegerValue() != this.initialMainColor ||
               this.config.getColorComponents().getIntegerValue() != this.initialComponentColor ||
               this.config.getKeyBind().matches(this.initialHotkeyValue) == false;
    }
}
