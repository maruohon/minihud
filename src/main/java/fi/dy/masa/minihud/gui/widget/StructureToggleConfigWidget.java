package fi.dy.masa.minihud.gui.widget;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.KeyBindConfigButton;
import fi.dy.masa.malilib.gui.config.ConfigWidgetContext;
import fi.dy.masa.malilib.gui.widget.ColorIndicatorWidget;
import fi.dy.masa.malilib.gui.widget.KeybindSettingsWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.config.BaseConfigOptionWidget;
import fi.dy.masa.minihud.config.StructureToggle;

public class StructureToggleConfigWidget extends BaseConfigOptionWidget<StructureToggle>
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

        this.booleanButton = new BooleanConfigButton(x, y + 1, -1, 20, config.getBooleanConfig());
        this.booleanButton.setActionListener((btn, mbtn) -> this.resetButton.setEnabled(this.config.isModified()));

        this.hotkeyButton = new KeyBindConfigButton(x, y + 1, 120, 20, config.getKeyBind(), ctx.getKeybindEditingScreen());
        this.hotkeyButton.setValueChangeListener(() -> this.resetButton.setEnabled(this.config.isModified()));

        this.settingsWidget = new KeybindSettingsWidget(x, y, 20, 20, config.getKeyBind(),
                                                        config.getDisplayName(), ctx.getDialogHandler());

        this.colorIndicatorWidgetMain = new ColorIndicatorWidget(x, y, 18, 18, this.config.getColorMain(), (newValue) -> {
            this.config.getColorMain().setIntegerValue(newValue);
            this.reAddSubWidgets();
        });
        this.colorIndicatorWidgetMain.addHoverString("minihud.gui.label.hover.structures_color_main");

        this.colorIndicatorWidgetComponents = new ColorIndicatorWidget(x, y, 18, 18, this.config.getColorComponents(), (newValue) -> {
            this.config.getColorComponents().setIntegerValue(newValue);
            this.reAddSubWidgets();
        });
        this.colorIndicatorWidgetComponents.addHoverString("minihud.gui.label.hover.structures_color_components");

        this.resetButton.setActionListener((btn, mbtn) -> {
            this.config.resetToDefault();
            this.resetButton.setEnabled(this.config.isModified());
            this.booleanButton.updateDisplayString();
            this.hotkeyButton.updateDisplayString();
        });
    }

    @Override
    public void reAddSubWidgets()
    {
        super.reAddSubWidgets();

        int x = this.getX() + this.getMaxLabelWidth() + 10;
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
        this.updateResetButton(x, y, this.config);

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
}
