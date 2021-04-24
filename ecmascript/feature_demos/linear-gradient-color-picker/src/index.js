import React, { useRef, useState } from "react";
import classNames from "classnames";
import ReactDOM from "react-dom";
import { Card, Input, Form, Row, Col, Select, InputNumber, Button } from "antd";
import "antd/dist/antd.css";
import "./style.css";

const ColorType = {
  LinearGradient: "linear-gradient",
  Common: "common"
};

const DefaultColor = {
  [ColorType.LinearGradient]: {
    type: ColorType.LinearGradient,
    angle: 0,
    stops: [
      { percentage: 0, color: "#FFFFFF" },
      { percentage: 100, color: "#000000" }
    ]
  },
  [ColorType.Common]: {
    type: ColorType.Common,
    color: "#333333"
  }
};

const trimIfString = (v) => {
  if (typeof v === "string") {
    return String.prototype.trim.call(v);
  } else {
    return v;
  }
};

const LinearGradientColorPicker = ({ value, onChange }) => {
  const [selectedStop, setSelectedStop] = useState(0);
  const stops = Array.isArray(value.stops) ? value.stops : [];

  const stopContainerRef = useRef(null);

  const handleClickLinearGradientStop = (idx) => (e) => {
    if (e) {
      e.preventDefault();
      e.stopPropagation();
    }
    setSelectedStop(idx);
  };

  const handleChangeField = ({ key, isEvent = false }) => (eOrValue) => {
    if (isEvent && !eOrValue) {
      return;
    }
    const fieldValue = trimIfString(isEvent ? eOrValue.target.value : eOrValue);
    const newColor = {
      ...value,
      [key]: fieldValue
    };
    typeof onChange === "function" && onChange(newColor);
  };

  const handleChangeStopField = ({ idx, key, isEvent = false }) => (
    eOrValue
  ) => {
    if (isEvent && !eOrValue) {
      return;
    }
    const fieldValue = trimIfString(isEvent ? eOrValue.target.value : eOrValue);
    const newStop = {
      ...stops[idx],
      [key]: fieldValue
    };
    const newStops = [...stops];
    newStops.splice(idx, 1, newStop);
    const newColor = {
      ...value,
      stops: newStops
    };
    typeof onChange === "function" && onChange(newColor);
  };

  const handleDeleteStop = ({ idx }) => () => {
    if (stops.length > 1) {
      const newStops = [...stops];
      newStops.splice(idx, 1);
      const newColor = {
        ...value,
        stops: newStops
      };
      typeof onChange === "function" && onChange(newColor);
      setSelectedStop(0);
    }
  };

  const handleAddStop = () => () => {
    const newStop = {
      color: "#FFFFFF",
      percentage: 100
    };
    const newStops = [...stops];
    const newIdx = newStops.length;
    newStops.push(newStop);
    const newColor = {
      ...value,
      stops: newStops
    };
    typeof onChange === "function" && onChange(newColor);
    setSelectedStop(newIdx);
  };

  const handleDragEnter = (e) => {
    e.preventDefault();
  };

  const handleDragOver = (e) => {
    e.preventDefault();
  };

  const handleDragStart = ({ idx }) => (e) => {
    e.dataTransfer.setData("text/plain", idx);
    setSelectedStop(idx);
  };

  const handleDrop = (e) => {
    e.preventDefault();
    const idx = parseInt(e.dataTransfer.getData("text/plain"), 10);
    if (stopContainerRef.current) {
      const stopContainer = stopContainerRef.current;
      const rect = stopContainer.getBoundingClientRect();
      const width = rect.width;
      const offset = e.clientX - rect.left;
      const newPercentage = Math.round((offset / width) * 100);
      handleChangeStopField({ key: "percentage", idx })(newPercentage);
    }
  };

  return (
    <React.Fragment>
      <Row gutter={[16, 16]}>
        <Col span={24} className="linear-gradient-first-row">
          <div style={{ flex: 1 }}>
            <Form.Item label="偏移" className="inline-item">
              <InputNumber
                style={{ width: "100%" }}
                value={value.stops[selectedStop].percentage}
                defaultValue={0}
                min={0}
                max={100}
                formatter={(value) => `${value}%`}
                parser={(value) => value.replace("%", "")}
                onChange={handleChangeStopField({
                  key: "percentage",
                  idx: selectedStop
                })}
              />
            </Form.Item>
          </div>
          <div className="linear-gradient-actions">
            <Button type="ghost" onClick={handleAddStop()}>
              添加
            </Button>
            <Button
              type="ghost"
              onClick={handleDeleteStop({ idx: selectedStop })}
            >
              删除
            </Button>
          </div>
        </Col>
      </Row>
      <Row gutter={[16, 16]}>
        <Col span={16}>
          <Form.Item label="颜色" className="inline-item">
            <Input
              defaultValue={"#FFFFFF"}
              value={value.stops[selectedStop].color}
              onChange={handleChangeStopField({
                key: "color",
                idx: selectedStop,
                isEvent: true
              })}
            />
          </Form.Item>
        </Col>
        <Col span={8}>
          <Input
            defaultValue={"#FFFFFF"}
            type="color"
            value={value.stops[selectedStop].color}
            onChange={handleChangeStopField({
              key: "color",
              idx: selectedStop,
              isEvent: true
            })}
          />
        </Col>
      </Row>
      <Row gutter={[16, 16]}>
        <Col span={14}>
          <Form.Item label="色点">
            <div
              ref={stopContainerRef}
              className="linear-gradient-stops-container"
              style={{
                background: `linear-gradient(to right, ${value.stops
                  .map((s) => `${s.color} ${s.percentage}%`)
                  .join(",")})`
              }}
              onDragEnter={handleDragEnter}
              onDragOver={handleDragOver}
              onDrop={handleDrop}
            >
              {value.stops.map((s, i) => (
                <div
                  key={i}
                  className={classNames("linear-gradient-stop", {
                    "linear-gradient-stop-selected": i === selectedStop
                  })}
                  style={{ left: `${s.percentage}%` }}
                  onClick={handleClickLinearGradientStop(i)}
                  draggable={true}
                  onDragStart={handleDragStart({ idx: i })}
                />
              ))}
            </div>
          </Form.Item>
        </Col>
        <Col span={10}>
          <Form.Item label="方向" className="inline-item">
            <InputNumber
              defaultValue={0}
              min={0}
              max={360}
              style={{ width: "100%" }}
              formatter={(value) => `${value}°`}
              parser={(value) => value.replace("°", "")}
              onChange={handleChangeField({ key: "angle" })}
            />
          </Form.Item>
        </Col>
      </Row>
    </React.Fragment>
  );
};

const CommonColorPicker = ({ value, onChange }) => {
  const handleChangeField = ({ key, isEvent = false }) => (eOrValue) => {
    if (isEvent && !eOrValue) {
      return;
    }
    const fieldValue = trimIfString(isEvent ? eOrValue.target.value : eOrValue);
    const newColor = {
      ...value,
      [key]: fieldValue
    };
    typeof onChange === "function" && onChange(newColor);
  };

  // TODO
  const isColorValid = true;

  return (
    <Row gutter={[16, 16]}>
      <Col span={16}>
        <Form.Item
          label="颜色"
          className="inline-item"
          validateStatus={isColorValid ? "success" : "error"}
          help={isColorValid ? "" : "请输入正确的颜色"}
        >
          <Input
            defaultValue={"#FFFFFF"}
            value={value.color}
            onChange={handleChangeField({ key: "color", isEvent: true })}
          />
        </Form.Item>
      </Col>
      <Col span={8}>
        <Input
          defaultValue={"#FFFFFF"}
          type="color"
          value={value.color}
          onChange={handleChangeField({ key: "color", isEvent: true })}
        />
      </Col>
    </Row>
  );
};

const CustomColorPicker = ({ value, onChange }) => {
  const handleChangeType = ({ isEvent = false } = {}) => (eOrValue) => {
    if (isEvent && !eOrValue) {
      return;
    }
    const type = trimIfString(isEvent ? eOrValue.target.value : eOrValue);
    if (type !== value.type) {
      const newColor = { ...DefaultColor[type] };
      typeof onChange === "function" && onChange(newColor);
    }
  };

  const isValid = true;

  return (
    <Card
      size="small"
      title="高级色彩选择器"
      className="custom-color-picker"
      extra={
        <Select
          defaultValue={value.type}
          onChange={handleChangeType()}
          style={{ width: "120px" }}
        >
          <Select.Option value={ColorType.Common}>纯色</Select.Option>
          <Select.Option value={ColorType.LinearGradient}>
            线性渐变
          </Select.Option>
        </Select>
      }
    >
      {!isValid && (
        <Card.Meta description={`颜色校验失败，请注意标红部分和错误提示`} />
      )}
      {value.type === ColorType.LinearGradient && (
        <LinearGradientColorPicker value={value} onChange={onChange} />
      )}
      {value.type === ColorType.Common && (
        <CommonColorPicker value={value} onChange={onChange} />
      )}
    </Card>
  );
};

const App = () => {
  const [value, setValue] = useState({
    type: ColorType.LinearGradient,
    angle: 90,
    stops: [
      {
        color: "rgba(0, 0, 0, 1)",
        percentage: 0
      },
      {
        color: "rgba(255, 255, 255, 1)",
        percentage: 80
      },
      {
        color: "red",
        percentage: 100
      }
    ]
  });
  return (
    <div className="app">
      <CustomColorPicker value={value} onChange={(v) => setValue(v)} />
    </div>
  );
};

ReactDOM.render(<App />, document.getElementById("root"));
