package minihud.mixin.render.world_gen_overlay;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.ModifyArg;

import net.minecraft.util.math.BlockPos;
import net.minecraft.world.gen.ChunkGeneratorOverworld;

import minihud.config.RendererToggle;
import minihud.data.DataStorage;

@Mixin(ChunkGeneratorOverworld.class)
public abstract class ChunkGeneratorOverworldMixin
{
    @ModifyArg(method = "populate",
               at = @At(value = "INVOKE",
                        target = "Lnet/minecraft/world/gen/feature/WorldGenDungeons;generate(Lnet/minecraft/world/World;Ljava/util/Random;Lnet/minecraft/util/math/BlockPos;)Z"))
    private BlockPos minihud_onDungeonGenerationAttempt(BlockPos position)
    {
        if (RendererToggle.SPAWNER_POSITIONS.isRendererEnabled())
        {
            DataStorage.INSTANCE.worldGenPositions.addDungeonSpawnerPosition(position);
        }

        return position;
    }
}
