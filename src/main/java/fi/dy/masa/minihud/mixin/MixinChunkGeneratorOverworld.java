package fi.dy.masa.minihud.mixin;

import java.util.Random;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.gen.ChunkGeneratorOverworld;
import net.minecraft.world.gen.feature.WorldGenDungeons;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;

@Mixin(ChunkGeneratorOverworld.class)
public class MixinChunkGeneratorOverworld
{
    @Redirect(method = "populate", at = @At(value = "INVOKE",
              target = "Lnet/minecraft/world/gen/feature/WorldGenDungeons;generate(Lnet/minecraft/world/World;Ljava/util/Random;Lnet/minecraft/util/math/BlockPos;)Z"))
    private boolean onDungeonGenerateAttempt(WorldGenDungeons worldGenDungeons, World worldIn, Random rand, BlockPos position)
    {
        boolean success = worldGenDungeons.generate(worldIn, rand, position);

        if (RendererToggle.OVERLAY_SPAWNER_POSITIONS.isRendererEnabled())
        {
            DataStorage.getInstance().addDungeonSpawnerPosition(position, success);
        }

        return success;
    }
}
