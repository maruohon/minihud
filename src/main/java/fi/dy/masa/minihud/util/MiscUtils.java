package fi.dy.masa.minihud.util;

import java.util.List;
import java.util.Optional;
import java.util.Random;
import javax.annotation.Nullable;
import net.minecraft.block.entity.AbstractFurnaceBlockEntity;
import net.minecraft.entity.passive.AxolotlEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtList;
import net.minecraft.recipe.AbstractCookingRecipe;
import net.minecraft.recipe.Recipe;
import net.minecraft.text.Style;
import net.minecraft.text.Text;
import net.minecraft.text.TranslatableText;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockBox;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.biome.source.BiomeAccess;
import net.minecraft.world.biome.source.SeedMixer;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.malilib.util.IntBoundingBox;
import fi.dy.masa.minihud.mixin.IMixinAbstractFurnaceBlockEntity;
import it.unimi.dsi.fastutil.objects.Object2IntMap;
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;

public class MiscUtils
{
    private static final Random RAND = new Random();
    private static final int[] AXOLOTL_COLORS = new int[] { 0xFFC7EC, 0x8C6C50, 0xFAD41B, 0xE8F7Fb, 0xB6B5FE };

    public static long bytesToMb(long bytes)
    {
        return bytes / 1024L / 1024L;
    }

    public static double intAverage(int[] values)
    {
        final int size = values.length;
        long sum = 0L;

        for (int i = 0; i < size; ++i)
        {
            sum += values[i];
        }

        return (double) sum / (double) values.length;
    }

    public static boolean canSlimeSpawnAt(int posX, int posZ, long worldSeed)
    {
        return canSlimeSpawnInChunk(posX >> 4, posZ >> 4, worldSeed);
    }

    public static boolean canSlimeSpawnInChunk(int chunkX, int chunkZ, long worldSeed)
    {
        long slimeSeed = 987234911L;
        long rngSeed = worldSeed +
                       (long) (chunkX * chunkX *  4987142) + (long) (chunkX * 5947611) +
                       (long) (chunkZ * chunkZ) * 4392871L + (long) (chunkZ * 389711) ^ slimeSeed;

        RAND.setSeed(rngSeed);

        return RAND.nextInt(10) == 0;
    }

    public static boolean isOverworld(World world)
    {
        return world.getDimension().isNatural();
    }

    public static boolean isStructureWithinRange(@Nullable BlockBox bb, BlockPos playerPos, int maxRange)
    {
        if (bb == null ||
            playerPos.getX() < (bb.getMinX() - maxRange) ||
            playerPos.getX() > (bb.getMaxX() + maxRange) ||
            playerPos.getZ() < (bb.getMinZ() - maxRange) ||
            playerPos.getZ() > (bb.getMaxZ() + maxRange))
        {
            return false;
        }

        return true;
    }

    public static boolean isStructureWithinRange(@Nullable IntBoundingBox bb, BlockPos playerPos, int maxRange)
    {
        if (bb == null ||
            playerPos.getX() < (bb.minX - maxRange) ||
            playerPos.getX() > (bb.maxX + maxRange) ||
            playerPos.getZ() < (bb.minZ - maxRange) ||
            playerPos.getZ() > (bb.maxZ + maxRange))
        {
            return false;
        }

        return true;
    }

    public static boolean areBoxesEqual(IntBoundingBox bb1, IntBoundingBox bb2)
    {
        return bb1.minX == bb2.minX && bb1.minY == bb2.minY && bb1.minZ == bb2.minZ &&
               bb1.maxX == bb2.maxX && bb1.maxY == bb2.maxY && bb1.maxZ == bb2.maxZ;
    }

    public static void addAxolotlTooltip(ItemStack stack, List<Text> lines)
    {
        NbtCompound tag = stack.getNbt();

        if (tag != null && tag.contains(AxolotlEntity.VARIANT_KEY, Constants.NBT.TAG_INT))
        {
            int variantId = tag.getInt(AxolotlEntity.VARIANT_KEY);

            if (variantId >= 0 && variantId < AxolotlEntity.Variant.VARIANTS.length)
            {
                AxolotlEntity.Variant variant = AxolotlEntity.Variant.VARIANTS[variantId];
                String variantName = variant.getName();
                TranslatableText labelText = new TranslatableText("minihud.label.axolotl_tooltip.label");
                TranslatableText valueText = new TranslatableText("minihud.label.axolotl_tooltip.value", variantName, variantId);

                if (variantId < AXOLOTL_COLORS.length)
                {
                    valueText.setStyle(Style.EMPTY.withColor(AXOLOTL_COLORS[variantId]));
                }

                lines.add(Math.min(1, lines.size()), labelText.append(valueText));
            }
        }
    }

    public static void addBeeTooltip(ItemStack stack, List<Text> lines)
    {
        NbtCompound stackTag = stack.getNbt();

        if (stackTag != null && stackTag.contains("BlockEntityTag", Constants.NBT.TAG_COMPOUND))
        {
            NbtCompound beTag = stackTag.getCompound("BlockEntityTag");
            NbtList bees = beTag.getList("Bees", Constants.NBT.TAG_COMPOUND);
            int count = bees.size();
            int babyCount = 0;

            for (int i = 0; i < count; i++)
            {
                NbtCompound beeTag = bees.getCompound(i);
                NbtCompound entityDataTag = beeTag.getCompound("EntityData");

                if (entityDataTag.contains("CustomName", Constants.NBT.TAG_STRING))
                {
                    String beeName = entityDataTag.getString("CustomName");
                    lines.add(Math.min(1, lines.size()), new TranslatableText("minihud.label.bee_tooltip.name", Text.Serializer.fromJson(beeName).getString()));
                }

                if (entityDataTag.contains("Age", Constants.NBT.TAG_INT) &&
                    entityDataTag.getInt("Age") + beeTag.getInt("TickInHive") < 0)
                {
                    ++babyCount;
                }
            }

            TranslatableText text;

            if (babyCount > 0)
            {
                text = new TranslatableText("minihud.label.bee_tooltip.count_babies", String.valueOf(count), String.valueOf(babyCount));
            }
            else
            {
                text = new TranslatableText("minihud.label.bee_tooltip.count", String.valueOf(count));
            }

            lines.add(Math.min(1, lines.size()), text);
        }
    }

    public static void addHoneyTooltip(ItemStack stack, List<Text> lines)
    {
        NbtCompound tag = stack.getNbt();

        if (tag != null && tag.contains("BlockStateTag", Constants.NBT.TAG_COMPOUND))
        {
            tag = tag.getCompound("BlockStateTag");
            String honeyLevel = "0";

            if (tag != null && tag.contains("honey_level", Constants.NBT.TAG_STRING))
            {
                honeyLevel = tag.getString("honey_level");
            }
            else if (tag != null && tag.contains("honey_level", Constants.NBT.TAG_INT))
            {
                honeyLevel = String.valueOf(tag.getInt("honey_level"));
            }

            lines.add(Math.min(1, lines.size()), new TranslatableText("minihud.label.honey_info.level", honeyLevel));
        }
    }

    public static int getFurnaceXpAmount(AbstractFurnaceBlockEntity be)
    {
        Object2IntOpenHashMap<Identifier> recipes = ((IMixinAbstractFurnaceBlockEntity) be).minihud_getUsedRecipes();
        World world = be.getWorld();
        double xp = 0.0;

        for (Object2IntMap.Entry<Identifier> entry : recipes.object2IntEntrySet())
        {
            Optional<? extends Recipe<?>> recipeOpt = world.getRecipeManager().get(entry.getKey());

            if (recipeOpt.isPresent() && recipeOpt.get() instanceof AbstractCookingRecipe recipe)
            {
                xp += entry.getIntValue() * recipe.getExperience();
            }
        }

        return (int) xp;
    }

    public static Biome getBiomeMasaOptimization(int blockX, int blockY, int blockZ,
                                                 BiomeAccess.Storage storage, long seed)
    {
        final int x = blockX - 2;
        final int y = blockY - 2;
        final int z = blockZ - 2;
        final int xBy4 = x >> 2;
        final int yBy4 = y >> 2;
        final int zBy4 = z >> 2;
        final double d = (double)(x & 3) / 4.0;
        final double e = (double)(y & 3) / 4.0;
        final double f = (double)(z & 3) / 4.0;
        int o = 0;
        double g = Double.POSITIVE_INFINITY;

        for (int i = 0; i < 8; ++i)
        {
            int xInc = (i & 4) >> 2;
            int yInc = (i & 2) >> 1;
            int zInc = (i & 1);
            int q = xBy4 + xInc;
            int r = yBy4 + yInc;
            int s = zBy4 + zInc;
            double h = d - xInc;
            double t = e - yInc;
            double u = f - zInc;
            double v = mixer(seed, q, r, s, h, t, u);

            if (g > v)
            {
                o = i;
                g = v;
            }
        }

        int finalX = xBy4 + ((o & 4) >> 2);
        int finalY = yBy4 + ((o & 2) >> 1);
        int finalZ = zBy4 +  (o & 1);

        return storage.getBiomeForNoiseGen(finalX, finalY, finalZ);
    }

    private static double mixer(long l, int i, int j, int k, double d, double e, double f)
    {
        long m = SeedMixer.mixSeed(l, i);
        m = SeedMixer.mixSeed(m, j);
        m = SeedMixer.mixSeed(m, k);
        m = SeedMixer.mixSeed(m, i);
        m = SeedMixer.mixSeed(m, j);
        m = SeedMixer.mixSeed(m, k);
        m = SeedMixer.mixSeed(m, l);
        m = SeedMixer.mixSeed(m, l);
        double g = biomeSomething(m);
        double h = biomeSomething(m);
        double n = biomeSomething(m);
        return MathHelper.square(f + n) + MathHelper.square(e + h) + MathHelper.square(d + g);
    }

    private static double biomeSomething(long l)
    {
        double d = (double)Math.floorMod(l >> 24, 1024) / 1024.0;
        return (d - 0.5) * 0.9;
    }
}
