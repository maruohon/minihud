package fi.dy.masa.minihud.gui.widget;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.KeyBindConfigButton;
import fi.dy.masa.malilib.gui.config.ConfigWidgetContext;
import fi.dy.masa.malilib.gui.widget.ColorIndicatorWidget;
import fi.dy.masa.malilib.gui.widget.KeybindSettingsWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.config.BaseConfigWidget;
import fi.dy.masa.minihud.config.StructureToggle;

public class StructureToggleConfigWidget extends BaseConfigWidget<StructureToggle>
{
    protected final StructureToggle config;
    protected final ImmutableList<Integer> initialHotkeyValue;
    protected final BooleanConfigButton booleanButton;
    protected final KeyBindConfigButton hotkeyButton;
    protected final KeybindSettingsWidget settingsWidget;
    protected final ColorIndicatorWidget colorIndicatorWidgetMain;
    protected final ColorIndicatorWidget colorIndicatorWidgetComponents;
    protected final boolean initialBooleanValue;
    protected final int initialMainColor;
    protected final int initialComponentColor;

    public StructureToggleConfigWidget(int x, int y, int width, int height, int listIndex, int originalListIndex,
                                      StructureToggle config, ConfigWidgetContext ctx)
    {
        super(x, y, width, 22, listIndex, originalListIndex, config, ctx);

        this.config = config;
        this.initialBooleanValue = config.isEnabled();
        this.initialMainColor = config.getColorMain().getIntegerValue();
        this.initialComponentColor = config.getColorComponents().getIntegerValue();
        this.initialHotkeyValue = config.getKeyBind().getKeys();

        this.booleanButton = new BooleanConfigButton(-1, 20, config.getBooleanConfig());
        this.booleanButton.setActionListener(() -> {
            this.config.getBooleanConfig().toggleBooleanValue();
            this.updateButtonStates();
        });

        this.hotkeyButton = new KeyBindConfigButton(120, 20, config.getKeyBind(), ctx.getKeybindEditingScreen());
        this.hotkeyButton.setValueChangeListener(this::updateButtonStates);

        this.settingsWidget = new KeybindSettingsWidget(config.getKeyBind(),
                                                        config.getDisplayName(), ctx.getDialogHandler());

        this.colorIndicatorWidgetMain = new ColorIndicatorWidget(18, 18, this.config.getColorMain(), (newValue) -> {
            this.config.getColorMain().setValue(newValue);
            this.reAddSubWidgets();
        });
        this.colorIndicatorWidgetMain.translateAndAddHoverString("minihud.gui.label.hover.structures_color_main");

        this.colorIndicatorWidgetComponents = new ColorIndicatorWidget(18, 18, this.config.getColorComponents(), (newValue) -> {
            this.config.getColorComponents().setValue(newValue);
            this.reAddSubWidgets();
        });
        this.colorIndicatorWidgetComponents.translateAndAddHoverString("minihud.gui.label.hover.structures_color_components");

        this.resetButton.setActionListener(() -> {
            this.config.resetToDefault();
            this.updateButtonStates();
        });
    }

    @Override
    public void reAddSubWidgets()
    {
        super.reAddSubWidgets();

        int x = this.getElementsStartPosition();
        int y = this.getY() + 1;
        int elementWidth = this.getElementWidth();

        this.booleanButton.setPosition(x, y);

        int w = this.booleanButton.getWidth();
        x += w + 2;
        this.hotkeyButton.setPosition(x, y);

        w = elementWidth - w - 66;
        this.hotkeyButton.setWidth(w);

        x += w + 2;
        this.settingsWidget.setPosition(x, y);

        x += this.settingsWidget.getWidth() + 4;
        this.colorIndicatorWidgetMain.setPosition(x, y + 2);
        x += 21;
        this.colorIndicatorWidgetComponents.setPosition(x, y + 2);

        x += 21;
        this.updateResetButton(x, y);
        this.updateButtonStates();

        this.addWidget(this.booleanButton);
        this.addWidget(this.hotkeyButton);
        this.addWidget(this.settingsWidget);
        this.addWidget(this.colorIndicatorWidgetMain);
        this.addWidget(this.colorIndicatorWidgetComponents);
        this.addWidget(this.resetButton);
    }

    @Override
    public boolean wasModified()
    {
        return this.config.isEnabled() != this.initialBooleanValue ||
               this.config.getColorMain().getIntegerValue() != this.initialMainColor ||
               this.config.getColorComponents().getIntegerValue() != this.initialComponentColor ||
               this.config.getKeyBind().getKeys().equals(this.initialHotkeyValue) == false;
    }

    protected void updateButtonStates()
    {
        this.booleanButton.setEnabled(this.config.getBooleanConfig().isLocked() == false);
        this.booleanButton.updateButtonState();
        this.booleanButton.updateHoverStrings();

        this.hotkeyButton.updateButtonState();
        this.hotkeyButton.updateHoverStrings();

        this.resetButton.setEnabled(this.config.isModified() && this.config.getBooleanConfig().isLocked() == false);
    }
}
