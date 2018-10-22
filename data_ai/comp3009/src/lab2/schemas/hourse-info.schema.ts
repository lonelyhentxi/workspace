export const HourseInfoSchema:{[key:string]:{label:string,selector:string,orSelector?:string}} = {
  title: {
    label: '房源标题',
    selector:".tab-cont .title",
  },
  price: {
    label: '价格',
    selector:".trl-item",
  },
  rental_manner: {
    label: '出租方式',
    selector:"div.tr-line:nth-child(3) > div:nth-child(1) > div:nth-child(1)",
  },
  house_type: {
    label: '户型',
    selector:"div.tr-line:nth-child(3) > div:nth-child(2) > div:nth-child(1)",
  },
  area: {
    label: '建筑面积',
    selector:"div.tr-line:nth-child(3) > div:nth-child(3) > div:nth-child(1)",
  },
  oriented: {
    label: '朝向',
    selector:"div.tr-line:nth-child(4) > div:nth-child(1) > div:nth-child(1)",
  },
  floor: {
    label: '楼层',
    selector:"div.tr-line:nth-child(4) > div:nth-child(2) > div:nth-child(1)",
  },
  decoration: {
    label: '装修',
    selector:"div.tr-line:nth-child(4) > div:nth-child(3) > div:nth-child(1)",
  },
  community:{
    label: "小区",
    selector:"div.trl-item2:nth-child(1) > div:nth-child(2)"
  },
  address: {
    label: '地址',
    selector:"div.trl-item2:nth-child(3) > div:nth-child(2)",
    orSelector:"div.trl-item2:nth-child(2) > div:nth-child(2)"
  },
};