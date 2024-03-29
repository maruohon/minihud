package minihud.config;

import java.util.Locale;
import java.util.Optional;
import java.util.stream.Collectors;
import com.google.common.collect.ImmutableList;

import malilib.config.option.BooleanConfig;
import malilib.config.option.ColorConfig;
import malilib.config.option.ConfigInfo;
import malilib.config.option.HotkeyConfig;
import malilib.input.KeyBind;
import malilib.input.callback.ToggleBooleanWithMessageKeyCallback;
import malilib.util.data.ModInfo;
import minihud.Reference;
import minihud.data.structure.StructureDataUtils;

public enum StructureToggle implements ConfigInfo
{
    DESERT_PYRAMID      ("desertPyramid",   "#30FFFF00", "#30FFFF00"),
    END_CITY            ("endCity",         "#30EB07EB", "#30EB07EB"),
    IGLOO               ("igloo",           "#300FAFE4", "#300FAFE4"),
    JUNGLE_TEMPLE       ("jungleTemple",    "#3099FF00", "#3099FF00"),
    MANSION             ("mansion",         "#30FF6500", "#30FF6500"),
    MINESHAFT           ("mineshaft",       "#30E0E000", "#30E0E000"),
    OCEAN_MONUMENT      ("oceanMonument",   "#3029E6EF", "#3029E6EF"),
    NETHER_FORTRESS     ("netherFortress",  "#30FC381D", "#30FC381D"),
    STRONGHOLD          ("stronghold",      "#30009999", "#30009999"),
    VILLAGE             ("village",         "#3054CB4E", "#3054CB4E"),
    SWAMP_HUT           ("swampHut",        "#30BE1DFC", "#300099FF"),

    UNKNOWN             ("unknown",         "#30FFFFFF", "#30FFFFFF");

    public static final ImmutableList<StructureToggle> VALUES = ImmutableList.copyOf(values());
    public static final ImmutableList<ColorConfig> COLOR_CONFIGS = getColorConfigs();
    public static final ImmutableList<BooleanConfig> TOGGLE_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(StructureToggle::getBooleanConfig).collect(Collectors.toList()));
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
        String commentKey = "minihud.structure_toggle.comment." + nameLower;
        String colorMainKey = "minihud.structure_toggle.color.main." + nameLower;
        String colorComponentsKey = "minihud.structure_toggle.color.components." + nameLower;

        this.toggleStatus.setNameTranslationKey(nameKey);
        this.toggleStatus.setPrettyNameTranslationKey(nameKey);
        this.toggleStatus.setCommentTranslationKey(commentKey);

        this.colorMain.setNameTranslationKey(colorMainKey);
        this.colorMain.setPrettyNameTranslationKey(colorMainKey);
        this.colorComponents.setNameTranslationKey(colorComponentsKey);
        this.colorComponents.setPrettyNameTranslationKey(colorComponentsKey);

        this.toggleHotkey.setNameTranslationKey(nameKey);
        this.toggleHotkey.setCommentTranslationKey(commentKey);
        this.toggleHotkey.getKeyBind().setCallback(new ToggleBooleanWithMessageKeyCallback(this.toggleStatus));
        this.toggleStatus.addValueChangeListener(StructureDataUtils::requestStructureDataUpdates);
    }

    public boolean isEnabled()
    {
        return this.toggleStatus.getBooleanValue();
    }

    public BooleanConfig getBooleanConfig()
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
    public ModInfo getModInfo()
    {
        return Reference.MOD_INFO;
    }

    @Override
    public String getName()
    {
        return this.toggleStatus.getName();
    }

    @Override
    public String getDisplayName()
    {
        return this.toggleStatus.getDisplayName();
    }

    @Override
    public Optional<String> getComment()
    {
        return this.toggleStatus.getComment();
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
