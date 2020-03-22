use crate::{material::Material, ray::Ray, vec3::Vec3, Material};

#[derive(Copy, Clone)]
pub struct HitRecord<'obj> {
    pub t: f32,
    pub p: Vec3,
    pub n: Vec3,
    pub material: &'obj Material,
}

#[derive(Copy, Clone)]
pub struct Sphere {
    pub center: Vec3,
    pub radius: f32,
    pub material: Material,
}


impl Sphere {
    pub fn new(center: Vec3, radius: f32, material: Material) -> Sphere {
        Sphere {
            center,
            radius,
            material,
        }
    }

    pub fn hit(&self, ray: &Ray, t_min: f32, t_max: f32) -> Option<HitRecord> {
        let oc = ray.origin - self.center;
        let a = ray.direction.dot(ray.direction);
        let b = oc.dot(ray.direction);
        let c = oc.dot(oc) - self.radius * self.radius;
    }
}
