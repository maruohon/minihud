package fi.dy.masa.minihud.config;

import java.util.Locale;
import java.util.stream.Collectors;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.ColorConfig;
import fi.dy.masa.malilib.config.option.ConfigInfo;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.input.KeyBind;
import fi.dy.masa.minihud.data.DataStorage;

public enum StructureToggle implements ConfigInfo
{
    OVERLAY_STRUCTURE_DESERT_PYRAMID    ("desertPyramid",   "#30FFFF00", "#30FFFF00"),
    OVERLAY_STRUCTURE_END_CITY          ("endCity",         "#30EB07EB", "#30EB07EB"),
    OVERLAY_STRUCTURE_IGLOO             ("igloo",           "#300FAFE4", "#300FAFE4"),
    OVERLAY_STRUCTURE_JUNGLE_TEMPLE     ("jungleTemple",    "#3099FF00", "#3099FF00"),
    OVERLAY_STRUCTURE_MANSION           ("mansion",         "#30FF6500", "#30FF6500"),
    OVERLAY_STRUCTURE_NETHER_FORTRESS   ("netherFortress",  "#30FC381D", "#30FC381D"),
    OVERLAY_STRUCTURE_OCEAN_MONUMENT    ("oceanMonument",   "#3029E6EF", "#3029E6EF"),
    OVERLAY_STRUCTURE_STRONGHOLD        ("stronghold",      "#30009999", "#30009999"),
    OVERLAY_STRUCTURE_VILLAGE           ("village",         "#3054CB4E", "#3054CB4E"),
    OVERLAY_STRUCTURE_WITCH_HUT         ("witchHut",        "#30BE1DFC", "#300099FF");

    public static final ImmutableList<StructureToggle> VALUES = ImmutableList.copyOf(values());
    public static final ImmutableList<ColorConfig> COLOR_CONFIGS = getColorConfigs();
    public static final ImmutableList<BooleanConfig> TOGGLE_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(StructureToggle::getToggleOption).collect(Collectors.toList()));
    public static final ImmutableList<HotkeyConfig> TOGGLE_HOTKEYS = ImmutableList.copyOf(VALUES.stream().map(StructureToggle::getHotkeyConfig).collect(Collectors.toList()));

    private final BooleanConfig toggleStatus;
    private final HotkeyConfig toggleHotkey;
    private final ColorConfig colorMain;
    private final ColorConfig colorComponents;

    StructureToggle(String name, String colorMain, String colorComponents)
    {
        this.toggleStatus = new BooleanConfig(name, false);
        this.toggleHotkey = new HotkeyConfig(name, "");
        this.colorMain       = new ColorConfig(name +  " Main", colorMain);
        this.colorComponents = new ColorConfig(name + " Components", colorComponents);

        String nameLower = name.toLowerCase(Locale.ROOT);
        String nameKey = "minihud.structure_toggle.name." + nameLower;
        String colorMainKey = "minihud.structure_toggle.color.main." + nameLower;
        String colorComponentsKey = "minihud.structure_toggle.color.components." + nameLower;

        this.toggleStatus.setNameTranslationKey(nameKey).setPrettyNameTranslationKey(nameKey);
        this.toggleStatus.setCommentTranslationKey("minihud.structure_toggle.comment." + nameLower);

        this.colorMain.setNameTranslationKey(colorMainKey).setPrettyNameTranslationKey(colorMainKey);
        this.colorComponents.setNameTranslationKey(colorComponentsKey).setPrettyNameTranslationKey(colorComponentsKey);

        this.toggleHotkey.getKeyBind().setCallback((action, key) -> { this.toggleStatus.toggleBooleanValue(); return true; });
        this.toggleStatus.setValueChangeCallback((newValue, oldValue) -> DataStorage.getInstance().getStructureStorage().requestStructureDataUpdates());
    }

    public boolean isEnabled()
    {
        return this.toggleStatus.getBooleanValue();
    }

    public BooleanConfig getToggleOption()
    {
        return this.toggleStatus;
    }

    public HotkeyConfig getHotkeyConfig()
    {
        return this.toggleHotkey;
    }

    public KeyBind getKeyBind()
    {
        return this.toggleHotkey.getKeyBind();
    }

    public ColorConfig getColorMain()
    {
        return this.colorMain;
    }

    public ColorConfig getColorComponents()
    {
        return this.colorComponents;
    }

    private static ImmutableList<ColorConfig> getColorConfigs()
    {
        ImmutableList.Builder<ColorConfig> builder = ImmutableList.builder();

        for (StructureToggle toggle : VALUES)
        {
            builder.add(toggle.getColorMain());
            builder.add(toggle.getColorComponents());
        }

        return builder.build();
    }

    @Override
    public String getName()
    {
        return this.toggleStatus.getName();
    }

    @Override
    public String getConfigNameTranslationKey()
    {
        return this.toggleStatus.getConfigNameTranslationKey();
    }

    @Nullable
    @Override
    public String getCommentTranslationKey()
    {
        return this.toggleStatus.getCommentTranslationKey();
    }

    @Override
    public boolean isModified()
    {
        return this.toggleStatus.isModified() ||
               this.toggleHotkey.isModified() ||
               this.colorMain.isModified() ||
               this.colorComponents.isModified();
    }

    @Override
    public void resetToDefault()
    {
        this.toggleStatus.resetToDefault();
        this.toggleHotkey.resetToDefault();
        this.colorMain.resetToDefault();
        this.colorComponents.resetToDefault();
    }
}
