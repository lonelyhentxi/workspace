use std::sync::Arc;

use itertools::Itertools;
use vulkano::buffer::{Buffer, BufferContents, BufferCreateInfo, BufferUsage};
use vulkano::command_buffer::allocator::{
    StandardCommandBufferAllocator, StandardCommandBufferAllocatorCreateInfo,
};
use vulkano::command_buffer::{AutoCommandBufferBuilder, CommandBufferUsage, CopyBufferInfo};
use vulkano::device::{Device, DeviceCreateInfo, QueueCreateInfo, QueueFlags};
use vulkano::instance::{Instance, InstanceCreateFlags, InstanceCreateInfo};
use vulkano::memory::allocator::{AllocationCreateInfo, MemoryTypeFilter, StandardMemoryAllocator};
use vulkano::sync::GpuFuture;
use vulkano::{VulkanLibrary, sync};

#[derive(BufferContents)]
#[repr(C)]
struct MyStruct {
    a: u32,
    b: u32,
}

fn main() {
    let library = VulkanLibrary::new().expect("no local Vulkan library found");
    let instance = Instance::new(
        library,
        InstanceCreateInfo {
            flags: InstanceCreateFlags::ENUMERATE_PORTABILITY,
            ..Default::default()
        },
    )
    .expect("failed to create instance");

    let physical_device = instance
        .enumerate_physical_devices()
        .expect("failed to enumerate physical devices")
        .next()
        .expect("no physical device found");

    let queue_family_index = physical_device
        .queue_family_properties()
        .iter()
        .position(|query_family_properties| {
            query_family_properties
                .queue_flags
                .contains(QueueFlags::GRAPHICS)
        })
        .expect("no graphics queue family found");

    let (device, mut queues) = Device::new(
        physical_device,
        DeviceCreateInfo {
            queue_create_infos: vec![QueueCreateInfo {
                queue_family_index: queue_family_index as u32,
                ..Default::default()
            }],
            ..Default::default()
        },
    )
    .expect("failed to create device");

    let queue = queues.next().expect("no queue found");

    let memory_allocator = Arc::new(StandardMemoryAllocator::new_default(device.clone()));

    {
        let data = 12i32;
        let _buffer = Buffer::from_data(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::UNIFORM_BUFFER,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_DEVICE
                    | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            data,
        )
        .expect("failed to create buffer");
    }

    {
        let data = MyStruct { a: 5, b: 69 };
        let buffer = Buffer::from_data(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::UNIFORM_BUFFER,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_DEVICE
                    | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            data,
        )
        .expect("failed to create buffer");

        let mut content = buffer.write().expect("failed to write to buffer");
        content.a *= 2;
        content.b = 9;
    }

    {
        let iter = (0..128).map(|_| 5u8);
        let buffer = Buffer::from_iter(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::UNIFORM_BUFFER,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_DEVICE
                    | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            iter,
        )
        .expect("failed to create buffer");

        let mut content = buffer.write().expect("failed to read from buffer");

        content[12] = 83;
        content[7] = 3;
    }

    {
        let source_content = (0..64).collect_vec();
        let source = Buffer::from_iter(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::TRANSFER_SRC,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_HOST
                    | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            source_content,
        )
        .expect("failed to create source buffer");

        let destination_content: Vec<i32> = (0..64).map(|_| 0).collect_vec();
        let destination = Buffer::from_iter(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::TRANSFER_DST,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_HOST
                    | MemoryTypeFilter::HOST_RANDOM_ACCESS,
                ..Default::default()
            },
            destination_content,
        )
        .expect("failed to create destination buffer");

        let command_buffer_allocator = Arc::new(StandardCommandBufferAllocator::new(
            device.clone(),
            StandardCommandBufferAllocatorCreateInfo::default(),
        ));

        let mut builder = AutoCommandBufferBuilder::primary(
            command_buffer_allocator.clone(),
            queue_family_index as u32,
            CommandBufferUsage::OneTimeSubmit,
        )
        .expect("failed to create command buffer builder");

        builder
            .copy_buffer(CopyBufferInfo::buffers(source.clone(), destination.clone()))
            .expect("failed to copy buffer");

        let command_buffer = builder.build().expect("failed to build command buffer");

        let fut = sync::now(device.clone())
            .then_execute(queue.clone(), command_buffer)
            .expect("failed to execute command buffer")
            .then_signal_fence_and_flush()
            .expect("failed to signal fence and flush");

        fut.wait(None).expect("failed to wait for fence");

        let src_content = source.read().expect("failed to read from source buffer");
        let dst_content = destination
            .read()
            .expect("failed to read from destination buffer");

        assert_eq!(&*src_content, &*dst_content);

        println!("Copy operation completed successfully");
    }
}
